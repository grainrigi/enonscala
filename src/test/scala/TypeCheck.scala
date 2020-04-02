import org.scalatest._
import parsing.Abssyn._
import parsing.Oper._
import parsing.Ty._
import typing.TypeCheck._

class TypeCheckTest extends FlatSpec {
  "変数" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy), VarExp("x"))== IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map(), VarExp("x"))
    }
  }

  "isEmpty" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntListTy),
      UOpExp(IsEmptyOp, VarExp("x")))== BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->IntTy),
        UOpExp(IsEmptyOp, VarExp("x")))
    }
  }

  "+" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(PlusOp, IntExp(1), VarExp("x")))== IntTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(PlusOp, IntExp(1), VarExp("x")))
    }
  }

  "比較" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(EqOp, IntExp(1), VarExp("x")))== BoolTy)
    assert(tcheck(Map(), Map("x"->IntTy),
      BOpExp(LtOp, IntExp(1), VarExp("x")))== BoolTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(EqOp, IntExp(1), VarExp("x")))
    }
  }

  "Cons" should "正しい型付け" in
  {
    assert(tcheck(Map(), Map(),
      BOpExp(ConsOp, IntExp(1), NilExp))== IntListTy)
    assertThrows[TypeError] {
      tcheck(Map(), Map("x"->BoolTy),
        BOpExp(ConsOp, VarExp("x"), NilExp))
    }
  }

  "if" should "正しい型付け" in
  {
    assert(
      tcheck(Map(), Map("x"->BoolTy),
        IfExp(VarExp("x"), IntExp(1), IntExp(2)))== IntTy)
    // non boolean condition clause
    assertThrows[TypeError](
      tcheck(Map(), Map("x"->IntTy),
        IfExp(VarExp("x"), IntExp(1), IntExp(2))))
    // return type mismatch
    assertThrows[TypeError](
      tcheck(Map(), Map("x"->BoolTy),
        IfExp(VarExp("x"), IntExp(1), VarExp("x"))))
  }

  "関数" should "正しい型付け" in
  {
    assert(
      tcheck(Map("f"->FuncTy(List(IntTy,BoolTy),IntListTy)),
        Map("x"->BoolTy),
        AppExp("f", List(IntExp(1), VarExp("x"))))== IntListTy)
    // non existent function
    assertThrows[TypeError] {
      tcheck(Map(), Map(), AppExp("f", List(IntExp(1))))
    }
    // arity mismatch
    assertThrows[TypeError](
      tcheck(Map("f"->FuncTy(List(IntTy,BoolTy),IntListTy)),
        Map(),
        AppExp("f", List(IntExp(1))))== IntListTy)
    // type mismatch
    assertThrows[TypeError](
      tcheck(Map("f"->FuncTy(List(IntTy,BoolTy),IntListTy)),
        Map(),
        AppExp("f", List(IntExp(1), NilExp)))== IntListTy)
  }

  "ブロック式" should "正しい型付け" in
  {
    assert(
      tcheck(Map(),Map("x" -> IntTy), 
        BlockExp(List(
          ValExp("y", IntExp(2)),
          BOpExp(PlusOp, VarExp("x"), VarExp("y"))
        ))
      ) == IntTy
    )
    assert(
      tcheck(Map(),Map(), 
        BlockExp(List(
          ValExp("y", IntExp(2)),
          VarExp("y"),
          ValExp("z", VarExp("y"))
        ))
      ) == UnitTy
    )
  }


  "例: sort" should "正しい型付け" in
  {
    val ds = Main.parseFileDefs("examples/sort.scala")
    tcheckDefs(ds)
    succeed
  }

  "例: qsort" should "正しい型付け" in
  {
    val ds = Main.parseFileDefs("examples/qsort.scala")
    tcheckDefs(ds)
    succeed
  }

}
