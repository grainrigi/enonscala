package regalloc

import codegen.CodegenBase._
import codegen.Asm._
import codegen.Asm

object RegAllocCoalesce {
  import RegAlloc._

  // レジスタ（変数）r が合弁によって，どのレジスタ変数に
  // 対応しているかを返す
  def rename(m: Map[Reg,Reg], r: Reg): Reg =
    m.get(r) match {
      case None => r
      case Some(r1) => rename(m, r1)
    }

  // r2をr1に合弁できるかを判定
  //  g: 干渉グラフ
  //  k: 色数
  //  仮定： r2が既彩色ならr1も既彩色
  //  仮定： レジスタ割付けに使用されるときは k=allRegs.size
  //  rが既彩色かは, allRegs.contains(r)で判定
  def canCoalesce (g: Graph[Reg], k: Int, r1: Reg, r2: Reg): Boolean =
    if(g.adjacent(r1).contains(r2)) false
    else (allRegs.contains(r1), allRegs.contains(r2)) match {
      case (true, true) => false // 既彩色節同士は干渉するので合弁できない
      case (false, false) => canCoalesceByBriggs(g, k, r1, r2)
      case _ => canCoalesceByGeorge(g, k, r1, r2)
    }

  def canCoalesceByBriggs(g: Graph[Reg], k: Int, r1: Reg, r2: Reg): Boolean = {
    // 仮に合弁したグラフを生成
    val gdash = g.merge(r1, r2)
    // スライドの条件式をそのまま記述
    gdash.nodes.filter(n => gdash.adjacent(n).contains(r1) && gdash.degree(n) >= k).size < k
  }

  def canCoalesceByGeorge(g: Graph[Reg], k: Int, r1: Reg, r2: Reg): Boolean = {
    // r1は必ず既彩色節で、r2は必ず既彩色節でない
    // スライドの条件式をそのまま記述
    g.adjacent(r2).filter(z => !(g.adjacent(z).contains(r1) || g.degree(z) < k || allRegs.contains(z))).size == 0
  }


  def coalesce (g: Graph[Reg], ms: List[(Reg,Reg)]): (Graph[Reg],Map[Reg,Reg]) =
    ms match {
      case Nil => (g, Map[Reg,Reg]())
      case (x,y)::ms => {
        val (g1, m1) = coalesce(g, ms)
        val x1 = rename(m1, x)
        val y1 = rename(m1, y)
        val (x2, y2) = if (allRegs.contains(x1)) (x1, y1) else (y1, x1)        
        if (canCoalesce (g1, allRegs.size, x2, y2)) {
          (g1.merge(x2, y2), m1+(y2->x2))
        } else
          (g1, m1)
      }
    }
  
  def codegen(code: Asm.Code): Code = {
    val g = Liveness.interGraph(code)
    val (coalescedG,m) =  coalesce(g,Liveness.moves(code))
    val (alloc, spills) = coloring(coalescedG)
    if (spills.isEmpty) {
      val allocx = alloc ++ m.mapValues(r => alloc(rename(m,r)))
      genBody(code, allocx, spills)
    } else {
      val code1 = code.flatMap(spillInstr(spills, _))
      val g1 = Liveness.interGraph(code1)
      val (coalescedG1,m1) =  coalesce(g1,Liveness.moves(code1))
      val (alloc1, spills1) = coloring(coalescedG1)
      if (!spills1.isEmpty) notSupported("spill")
      val allocx1 = alloc1 ++ m1.mapValues(r => alloc1(rename(m1,r)))
      genBody(code1, allocx1, spills)
    }
  }

  def codegenDef(d: Def): Def  = Def (d.name, d.numArgs, codegen(d.body))

}
