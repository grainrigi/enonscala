package parsing

object Abssyn {
  import Oper._
  import Ty._

  type Var = String
  
  trait Exp
  case class VarExp(s: Var) extends Exp
  case class IntExp(i: Int) extends Exp
  case object NilExp extends Exp
  case object UnitExp extends Exp
  case class UOpExp(o: UOp, e: Exp) extends Exp       // 課題(a)では必要ない
  case class BOpExp(o: BOp, e1: Exp, e2: Exp) extends Exp
  case class IfExp(e: Exp, e1: Exp, e2: Exp) extends Exp
  case class AppExp(f: Var, es: List[Exp]) extends Exp // 課題(a)では必要ない
  case class ValExp(x: Var, e: Exp) extends Exp
  case class BlockExp(es: List[Exp]) extends Exp

  case class Def(name: Var, args: List[(Var,Ty)], rtype: Ty, body: Exp)
  case class Prog(funcs: List[Def])
}
