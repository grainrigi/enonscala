import org.scalatest._
import codegen.Asm._
import codegen._
import regalloc._


class RegAllocCoalesceTest extends FlatSpec with RegAllocBehavior {
  import regalloc.RegAllocCoalesce._
  def codegenFileDefs(s: String) = Main.codegenFileDefsCoalesce(s)

  def graphOf(ns: Set[Reg], es:Set[(Reg,Reg)]): Graph[Reg] =
    new Graph(ns, es ++ es.map{ case (x,y) => (y,x) })

  val g1 = graphOf(
    Set("x", "y", "z"),
    Set(("x","y")))

  "例: g1" should "正しい合弁判定" in
  {
    assert(canCoalesce(g1, 2, "y", "z"))
    assert(!canCoalesce(g1, 2, "x", "y"))
  }

  val g2 = graphOf(
    Set("a", "b", "c", "d", "e", "f"),
    Set(
      ("a","b"),
      ("b","c"),
      ("c","a"),
      ("b","d"),
      ("b","e"),
      ("c","d"),
      ("c","e"),
      ("d","f")))

  "例: g2 (Briggs)" should "正しい合弁判定" in
  {
    assert(!canCoalesce(g2, 2, "d", "e"))    
    assert(canCoalesce(g2, 3, "d", "e"))
  }

  val g3 = graphOf(
    Set("%rax", "%rdi", "%rsi", "0", "1"),
    Set(
      ("0","%rdi"),
      ("0","%rsi"),
      ("0","1")))

  "例: g3 (George)" should "正しい合弁判定" in
  {
    assert(canCoalesce(g3, 2, "%rax", "0"))
  }

  val g4 = graphOf(
    Set("%rax", "%rdi", "%rsi", "0", "1", "2"),
    Set(
      ("0","%rdi"),
      ("0","%rsi"),
      ("0","1"),
      ("1","2")))

  "例: g4 (George)" should "正しい合弁判定" in
  {
    assert(!canCoalesce(g4, 2, "%rax", "0"))
  }

  val g5 = graphOf(
    Set("0", "1", "2", "3", "4", "x", "y"),
    Set(
      ("0","1"),
      ("0","2"),
      ("1","2"),
      ("1","x"),
      ("1","y"),
      ("2","x"),
      ("2","y"),
      ("3","4"),
      ("3","x"),
      ("3","y"),
      ("4", "x"),
      ("4", "y")
    ))

  "例: g5 (Briggs)" should "正しい合弁判定" in
  {
    assert(!canCoalesce(g5, 2, "x", "y"))    
    assert(canCoalesce(g5, 3, "x", "y"))
  }

    val g6 = graphOf(
    Set("0", "1", "2", "3", "x", "y", "4"),
    Set(
      ("0","1"),
      ("0","2"),
      ("0","3"),
      ("1","2"),
      ("1","3"),
      ("1","x"),
      ("2","3"),
      ("2","y"),
      ("3","x"),
      ("3","y"),
      ("4", "x")))

  "例: g6 (Briggs)" should "正しい合弁判定" in
  {
    assert(!canCoalesce(g6, 3, "x", "y"))
    assert(canCoalesce(g6, 4, "x", "y"))    
  }

}
