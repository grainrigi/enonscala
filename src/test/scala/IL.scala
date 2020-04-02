import org.scalatest._
import parsing.Abssyn._
import parsing.Oper._
import codegen.ILExec._
import codegen._

class ILTest extends FlatSpec {

  def evalCV(fenv: FEnv, env: Env, cv: (IL.Code, IL.Val)): Value = 
    evalVal(exec(fenv, env, cv._1), cv._2)

  "変数" should "正しい値" in
  {
    assert(evalCV(Map(), Map("x"->IntValue(1)), IL.trans(VarExp("x")))== IntValue(1))
  }

  "Arithmetic Operations" should "正しい値" in
  {
    assert(evalCV(Map(), Map("x"->IntValue(1)),
      IL.trans(BOpExp(PlusOp, IntExp(1), VarExp("x"))))== IntValue(2))
    assert(evalCV(Map(), Map("x"->IntValue(1)),
      IL.trans(BOpExp(MinusOp, IntExp(1), VarExp("x"))))== IntValue(0))
    assert(evalCV(Map(), Map("x"->IntValue(2)),
      IL.trans(BOpExp(TimesOp, IntExp(4), VarExp("x"))))== IntValue(8))
    assert(evalCV(Map(), Map("x"->IntValue(2)),
      IL.trans(BOpExp(DivideOp, IntExp(4), VarExp("x"))))== IntValue(2))
  }

  "算術式" should "正しい値" in
  {
    assert(evalCV(Map(), Map("x"->IntValue(1),"y"->IntValue(2)),
      IL.trans(
        BOpExp(PlusOp,
          BOpExp(TimesOp, IntExp(1), VarExp("x")),
          BOpExp(TimesOp, VarExp("y"), VarExp("y")))))
          == IntValue(5))
  }

  "関数適用" should "正しい値" in
  {
    val (c, v) = IL.trans(BOpExp(ConsOp, VarExp("x"), VarExp("y")))
    assert(
      evalCV(Map("f"->FValue(List("x","y"), c, v)),
        Map("x"->ListValue(List(2))),
        IL.trans(AppExp("f", List(IntExp(1), VarExp("x")))))== ListValue(List(1,2)))
  }

  "if-else" should "正しい値" in
  {
    assert(
      evalCV(Map(), Map("x"->IntValue(1)),
        IL.trans(IfExp(BOpExp(EqOp, VarExp("x"), IntExp(1)), IntExp(5), IntExp(10))))== IntValue(5))
    assert(
      evalCV(Map(), Map("x"->IntValue(1)),
        IL.trans(IfExp(BOpExp(EqOp, VarExp("x"), IntExp(2)), IntExp(5), IntExp(10))))== IntValue(10))
  }

  "変数隠蔽" should "正しい値" in
  {
    val il = IL.trans(BlockExp(List(
            ValExp("x", IntExp(2)),
            BOpExp(PlusOp, VarExp("x"), BlockExp(List(
              ValExp("x", IntExp(3)),
              BOpExp(PlusOp, VarExp("x"), BlockExp(List(
                ValExp("x", IntExp(4)),
                VarExp("x")
              )))))))))
    assert(
      evalCV(Map(), Map("x"->IntValue(1)),il) == IntValue(9)
    )
  }

  "例: arith (x-y) * z" should "正しい値" in
  {
    val ds= Main.transFileDefs("examples/arith.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("test", List(IntExp(4),IntExp(2),IntExp(3))))) == IntValue(6))
  }


  "例: fact" should "正しい値" in
  {
    val ds = Main.transFileDefs("examples/fact.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("fact", List(IntExp(4))))) == IntValue(24))
  }
  

  "例: sort" should "正しい値" in
  {
    val ds = Main.transFileDefs("examples/sort.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map("x"->ListValue(List(3,2,1))), IL.trans(AppExp("sort", List(VarExp("x"))))) == ListValue(List(1,2,3)))

  }

  "例: qsort" should "正しい値" in
  {
    val ds = Main.transFileDefs("examples/qsort.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map("x"->ListValue(List(3,2,1,3,5,3,1,4))), IL.trans(AppExp("qsort", List(VarExp("x"))))) == ListValue(List(1,1,2,3,3,3,4,5)))

  }

  "例: primes" should "正しい値" in
  {
    val ds = Main.transFileDefs("examples/primes.scala")
    val fenv = defs2env(ds)
    assert(
      evalCV(fenv, Map(), IL.trans(AppExp("primes", List(IntExp(20))))) ==
        ListValue(List(2,3,5,7,11,13,17,19)))

  }



}
