package regalloc

import parsing.Abssyn.Var
import codegen.Asm._
import codegen.CodegenBase._
import codegen.Asm

object RegAlloc {

  def deg(g: Graph[Reg], r: Reg): Int =
    if (allRegs.contains(r)) Int.MaxValue else g.degree(r)

  // グラフ中で最小の次数を持つ節を返す
  // ただし，既彩色の節の次数は無限大(実際は,Int.MaxValue)と扱う
  def minDegree(g : Graph[Reg]): Reg =
    g.nodes.minBy(r => deg(g,r))

  def getUnused(regs: List[Reg], used: Set[Reg]): Option[Reg] =
    regs match {
      case Nil => None
      case r::rs =>
        if (used.contains(r)) getUnused(rs, used)
        else Some(r)
    }

  // グラフ彩色
  // 仮定：グラフ g が precolored レジスタを一つは含む
  // 結果：
  //   Map[Reg,Reg]：変数（仮想レジスタ）から実レジスタへの対応
  //   Set[Reg]：    スピルした変数（仮想レジスタ）の集合
  def coloring (g: Graph[Reg]): (Map[Reg, Reg], Set[Reg]) = {
    val x = minDegree(g)
    if (allRegs.contains(x)) {
      // 既彩色節しかない
      val m = g.nodes.map(r => (r,r)).toMap
      (m, Set())
    } else {
      val i = g.degree(x)
      // 潜在的スピル if i >= numRegs
      val (m, spills) = coloring(g.removeNode(x))
      val usedRegs = g.adjacent(x).collect(m)
      getUnused(allRegs, usedRegs) match {
        case Some(r) => (m + (x -> r), spills)
        case None =>
          if (i < allRegs.size)
            throw(new Error())             // アルゴリズムの性質から発生しない
          else
            (m,  spills + x)    // 実スピル
      }
    }
  }

  def raReg(m:Map[Reg,Reg], r: Reg) =  m.getOrElse(r,r)

  def raOperand(m: Map[Reg,Reg], ri: Operand) =
    ri match {
      case RegOpd(x) => RegOpd(raReg(m, x))
      case IntOpd(_) => ri
    }

  def raMem(regm: Map[Reg,Reg], mem: Mem) =
    mem match {
      case  MemReg(r, i) => MemReg(raReg(regm, r), i)
      case _ => mem
    }

  // 仮想レジスタを実レジスタに置き換える
  def raInstr(m: Map[Reg,Reg], i: Instr): Instr =
    i match {
      case Movq (s, d) => Movq (raOperand(m,s), raReg(m,d))
      case Loadq (s, d) => Loadq (raMem(m, s), raReg(m,d))
      case Storeq (s, d) => Storeq (raReg(m,s), raMem(m, d))
      case Addq(s,  d) => Addq(raOperand(m,s), raReg(m,d))
      case Subq(s,  d) => Subq (raOperand(m,s), raReg(m,d))
      case Imulq(s, d) => Imulq (raOperand(m,s), raReg(m,d))
      case Cmpq(s1, s2) => Cmpq(raOperand(m,s1), raReg(m,s2))
      case Idivq(s) => Idivq (raReg(m,s))
      case Incq(d) => Incq (raReg(m,d))
      case Decq(d) => Decq (raReg(m,d))
      case _ => i
    }

  def spillSrc(rs: Set[Reg], r: Reg) : Code=
    if (rs.contains(r)) 
      List(Loadq (MemTmp(r), r))
    else
      List()

  def spillDst(rs: Set[Reg], r: Reg) : Code =
    if (rs.contains(r)) 
      List(Storeq (r, MemTmp(r)))
    else
      List()

  // スピルした変数（仮想レジスタ）をメモリアクセスに置き換える
  def spillInstr(rs: Set[Reg], i: Instr) : Code =
    i match {
      case LabelInstr(_) | Jmp(_) | Jcc(_,_) => List(i)
      case _ =>
        i.use.flatMap(spillSrc(rs,_)).toList++
        List(i)++
        i.defR.flatMap(spillDst(rs,_)).toList
    }

  // スピルした変数をスタックに割付る
  def assignSpill(spillMap: Map[Var,Int], mem: Mem): Mem =
    mem match {
      case MemReg(_, _) => mem
      case MemTmp(s) => MemBp(spillMap(s))
      case MemBp(_) => throw(new Error())
    }

  def assignSpillInstr(spillMap: Map[Var,Int], i: Instr): Instr =
    i match {
      case Loadq (s, d) => Loadq (assignSpill(spillMap,s), d)
      case Storeq (s, d) => Storeq(s, assignSpill(spillMap, d))
      case _ => i
    }

  /* プロロークとエピローグをcode に付加する
     プロローク 
      pushq %rbp
      movq %rsp,%rbp
      push 使用するCallee-save レジスタをpush 
      ...
      push
      subq n, %rsp  n はスピルした変数の数 * 8
                    ただし, フレームが16バイト境界に
                    なるように調整が必要

      エピローグ
      addq n, %rsp
      pop 使用したCallee-save レジスタをpop
      ...
      pop
      popq %rbp
      retq
 */

  def genProEpi(code: Code, n: Int, calleeRegs: Set[Reg]): Code = {
    val regs = calleeRegs.toList
    val parityRegs = regs.size % 2 // calle-saveレジスタのpushqにより16バイト境界が崩れたかどうか
    // Mac OS Xでは，%rspは16バイト境界
    val s = (n + parityRegs + 1) / 2 * 16 - parityRegs * 8
    val padded = (n + parityRegs) % 2 != 0
    val pro =
      List(
        Pushq("%rbp"),
        Movq(RegOpd("%rsp"), "%rbp")) ++ regs.map(Pushq(_)) ++
        List(Subq(IntOpd(s), "%rsp")) ++ 
        // 未使用領域が存在する際にそこを0で埋める
        (if (padded) List(Movq(IntOpd(0), "0(%rsp)")) else Nil)
    val epi =
      List(
        Addq(IntOpd(s), "%rsp")) ++ regs.reverseMap(Popq(_)) ++
        List(Popq("%rbp"), Retq)
    pro ++ code ++ epi
  }

  /*
   関数本体のコードを生成
   - alloc: レジスタ割付け情報
   - spills: スピルした変数の集合
  */
  def genBody(code: Asm.Code, alloc: Map[Reg,Reg], spills: Set[Reg]): Code = {
    val calleeRegs: Set[Reg] = alloc.values.toSet & calleeSaveRegs.toSet
    val numCalleeRegs = calleeRegs.size
    val spillsList = spills.toList
    val numSpills = spillsList.length
    val spillAssign =
      (spillsList zip List.range(0,numSpills)).map{
        case (x, i) => (x, numCalleeRegs + i)
      }
//    Emit.emitCode(code)
    // 静的に生存していない変数を0で埋める
    val code1 = clearDeadRegs(code.map(raInstr(alloc,_)))
//    Emit.emitCode(code1)    
    val code2 = code1.map(assignSpillInstr(spillAssign.toMap, _))
//    Emit.emitCode(code2)
    genProEpi(code2, numSpills, calleeRegs)
  }

  def clearDeadRegs(code: Asm.Code): Code = {
      // callee-saveレジスタのみを対象にする
      // ゴミがスタックに保存される可能性があるのは、ゴミの入ったcallee-saveレジスタがcallqの先で保存されるとき
      // よって、callqの直前で、生きていないcallee-saveレジスタを0にする

      val liveList = Liveness.livenessList(code)
      var regSet = Asm.calleeSaveRegs.map(x => x:Reg).toSet
      var livingCalleSaveRegs: Set[Reg] = liveList.head & regSet

      code.zip(liveList).flatMap(x => x match {
          case (Asm.Callq(_,_), l) => {
            val dying = livingCalleSaveRegs -- (l & regSet)
            livingCalleSaveRegs = l & regSet
            dying.map(r => Asm.Movq(IntOpd(0), r)).toList :+ x._1
          }
          case _ => List(x._1)
      })
  }

  def codegen(code: Asm.Code): Code = {
//    Emit.emitCode(code)
    val g = Liveness.interGraph(code)
    val (alloc, spills) = coloring(g)
    if (spills.isEmpty)
      genBody(code, alloc, spills)
    else {
      val code1 = code.flatMap(spillInstr(spills, _))
//      Emit.emitCode(code1)
      val g1 = Liveness.interGraph(code1)
      val (alloc1, spills1) = coloring(g1)
      if (!spills1.isEmpty) notSupported("spill")
      genBody(code1, alloc1, spills)
    }

  }

  def codegenDef(d: Def): Def  = Def (d.name, d.numArgs, codegen(d.body))

}
