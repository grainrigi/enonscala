package parsing

object Oper {
  trait BOp                         // 二項(Binary)演算子
  case object PlusOp extends BOp
  case object MinusOp extends BOp
  case object TimesOp extends BOp
  case object DivideOp extends BOp
  case object EqOp extends BOp       // E == E
  case object LtOp extends BOp       // E < E
  case object ConsOp extends BOp     // E :: C

  trait UOp                        // 課題(a)では必要ない
  case object IsEmptyOp extends UOp
  case object HeadOp extends UOp
  case object TailOp extends UOp
}

object Ty {                        // 課題(a)では必要ない
  trait Ty
  case object UnitTy extends Ty
  case object IntTy extends Ty
  case object BoolTy extends Ty
  case object IntListTy extends Ty
}
