import org.scalatest._
import codegen.IL._
import parsing.Oper._
import codegen.AsmExec._
import codegen._

trait RegAllocBehavior { this: FlatSpec =>

  def codegenFileDefs(s: String) : List[Asm.Def]

  val env0 = Asm.allRegs.map((_,0)).toMap + ("%rsp"-> 100000, "%rbp" -> 0)

  "例: fact" should "正しい値" in
  {
    val ds =  codegenFileDefs("examples/fact.scala")
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, env0 + (Asm.argRegs(0) ->4), Map(), 16000, List(Asm.Callq("fact", 1)))
    assert(env2(Asm.retReg) == 24) 
  }

  "例: arith (x-y) * z" should "正しい値" in
  {
    val ds =  codegenFileDefs("examples/arith.scala")
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, env0 + (Asm.argRegs(0) ->4, Asm.argRegs(1) -> 2, Asm.argRegs(2) -> 3), Map(), 16000, List(Asm.Callq("test", 3)))
    assert(env2(Asm.retReg) == 6)
  }

  def memOf(l: List[Int]): (Mem, Int, Int) = {
    val a0 = 16000
    def loop(l: List[Int]): (Mem, Int, Int) =
      l match {
        case Nil => (Map(), a0, 0)
        case i::l => {
          val (mem, alloc, a) = loop(l)
          val mem1 = mem + (alloc -> i)
          val mem2 = mem1 + (alloc + 8 -> a)
          (mem2, alloc+16, alloc)
        }
      }
    val (mem, alloc, a) = loop(l)
    (mem, a, alloc)
  }

  "例: sort" should "正しい値" in
  {
    val ds =  codegenFileDefs("examples/sort.scala")
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1) = memOf(List(3,2,1))
    val (env2, mem2, _) =
      execCode(fenv, env0 + (Asm.argRegs(0) ->a0), mem, a1, List(Asm.Callq("sort", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(1,2,3)) 
  }

  "例: qsort" should "正しい値" in
  {
    val ds = codegenFileDefs("examples/qsort.scala")
    val fenv = AsmExec.defs2env(ds)
    val (mem, a0, a1) = memOf(List(3,2,1,3,5,3,1,4))
    val (env2, mem2, _) =
      execCode(fenv, env0 + (Asm.argRegs(0) ->a0), mem, a1, List(Asm.Callq("qsort", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(1,1,2,3,3,3,4,5))
  }

  "例: primes" should "正しい値" in
  {
    val ds = codegenFileDefs("examples/primes.scala")
    val fenv = AsmExec.defs2env(ds)
    val (env2, mem2, _) =
      execCode(fenv, env0 + (Asm.argRegs(0) ->20), Map(), 16000, List(Asm.Callq("primes", 1)))
    assert(mem2list(mem2, env2(Asm.retReg)) == List(2,3,5,7,11,13,17,19)) 
  }
}
