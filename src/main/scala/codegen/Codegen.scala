package codegen

import parsing.Oper._
import CodegenBase._
import Asm._

object Codegen {
  def v2operand (v:IL.Val): Operand =
    v match {
      case IL.IntVal(i) => IntOpd(i)
      case IL.VarVal(x) => RegOpd(x)
      case IL.NilVal => IntOpd(0)
      case IL.UnitVal => IntOpd(0xCCCCCCCC) // UnitValは参照されないはずなのでこの値は闇に消えるはずである(Unitを返す関数が無駄にraxに積み込むこととなる)
    }

  def genArith (op: (Operand, Reg) => Instr, d: Reg, v1: IL.Val, v2: IL.Val): List[Instr] = {
    if(v2operand(v1) == RegOpd(d)) List(op(v2operand(v2), d))
    else if(v2operand(v2) == RegOpd(d)) {
      val y = freshVar()
      List(Movq(RegOpd(d), y), Movq(v2operand(v1), d), op(RegOpd(y), d))
    }
    else List(Movq(v2operand(v1), d), op(v2operand(v2), d))
  }

  // 代入文 d <- e のコードを生成
  // 変数 d が式 e に現れないことを仮定してよい
  def genExp (e: IL.Exp, d: Reg) : List[Instr] =
    e match {
      case IL.ValExp(v) =>
        List(Movq(v2operand(v), d))
      case IL.BOpExp(PlusOp, v1, v2) =>
        genArith(Addq, d, v1, v2)
      case IL.BOpExp(MinusOp, v1, v2) =>
        genArith(Subq, d, v1, v2)
      case IL.BOpExp(TimesOp, v1, v2) =>
        genArith(Imulq, d, v1, v2)
      case IL.BOpExp(DivideOp, v1, v2) => {
        val r = freshVar()
        List(Movq(v2operand(v1), "%rax"), Cqto,
          Movq(v2operand(v2), r), Idivq(r), Movq(RegOpd("%rax"), d))
      }
      case IL.CallExp(f, vs) => {
        if (vs.length > 6) throw(new Error())
        val movs = vs.zip(List("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"))
                     .map(x => Movq(v2operand(x._1), x._2))
        movs ++ List(Callq(f, vs.length), Movq(RegOpd("%rax"), d))
      }
      case IL.UOpExp(HeadOp, IL.VarVal(x)) => 
        List(Loadq(MemReg(x, 0), d))
      case IL.UOpExp(TailOp, IL.VarVal(x)) => 
        List(Loadq(MemReg(x, 8), d))
      case IL.UOpExp(HeadOp, v1) => notSupported("HeadOp")
      case IL.UOpExp(TailOp, v1) => notSupported("TailOp")
      case IL.BOpExp(ConsOp, v1, v2) => 
        List(
          Movq(v2operand(v1), argRegs(0)),
          Movq(v2operand(v2), argRegs(1)),
          Callq("cons", 2),
          Movq(RegOpd(retReg),d))
    }

  def cmp(o1: Operand, o2: Operand): List[Instr] = {
    val r = freshVar()
    List(Movq(o1, r), Cmpq(o2, r))
  }

  def genCExp (e:IL.CExp) : (List[Instr], CC) =
    e match {
      case IL.EqCExp(v1, v2) => 
        (cmp(v2operand(v1), v2operand(v2)), EqCC)
      case IL.LtCExp(v1, v2) => 
        (cmp(v2operand(v1), v2operand(v2)), LtCC)
      case IL.IsEmptyCExp(v1) =>
        (cmp(v2operand(v1), v2operand(IL.IntVal(0))), EqCC)
    }

  def genInstr (s:IL.Instr) : Code =
    s match {
      case IL.AssignInstr(x, e) => genExp(e, x)
      case IL.LabelInstr(l) => List(LabelInstr(l))
      case IL.GotoInstr(l) => List(Jmp(l))
      case IL.IfGotoInstr(ce, l) => {
        val (c, cc) = genCExp(ce) 
        c++List(Jcc(cc,l))
      }
    }

  def genCode (c: IL.Code) : Code = c.flatMap(genInstr(_))

  def genDef (d: IL.Def) : Def = {
    val c1 =
      (d.args zip argRegs).map { case (x, r) => Movq(RegOpd(r), x) }
    val c2 = genCode(d.code)
    val c3 = List(Movq(v2operand(d.result), retReg))
    Def(d.name, d.args.length, c1++c2++c3)
  }
}
