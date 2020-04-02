package codegen

object ILExec {
  import parsing.Abssyn.Var
  import CodegenBase._
  import parsing.Oper._
  import IL._

  // 値  
  trait Value
  case object VoidValue extends Value
  case class IntValue(i: Int) extends Value
  case class ListValue(l: List[Int]) extends Value

  type Env = Map[Var, Value]

  case class FValue(xs: List[Var], c: Code, v: Val)
  type FEnv = Map[Var, FValue]

  def evalVal (env: Env, v: Val) : Value = 
    v match {
      case VarVal(x) => env(x)
      case IntVal(i) => IntValue(i)
      case NilVal => ListValue(Nil)
      case UnitVal => VoidValue
    }

  def evalExp (fenv: FEnv,  env: Env, e: Exp) : Value =
    e match {
      case ValExp(v) => evalVal(env, v)
      case BOpExp(o, v1, v2) => {
        val w1 = evalVal(env, v1)
        val w2 = evalVal(env, v2)
          (o,w1,w2) match {
          case (PlusOp, IntValue(i1), IntValue(i2)) => IntValue(i1 + i2)
          case (MinusOp, IntValue(i1), IntValue(i2)) => IntValue(i1 - i2)
          case (TimesOp, IntValue(i1), IntValue(i2)) => IntValue(i1 * i2)
          case (DivideOp, IntValue(i1), IntValue(i2)) => IntValue(i1 / i2)
          case (ConsOp, IntValue(i), ListValue(l)) => ListValue(i::l)
          case _ => notSupported("BOp")
        }
      }
      case UOpExp(o, v1) => {
        val w1 = evalVal(env, v1)
          (o,w1) match {
          case (HeadOp, ListValue(i::l)) => IntValue(i)
          case (TailOp, ListValue(i::l)) => ListValue(l)
          case _ => notSupported("UOp")
        }
      }
      case CallExp(f, vs) => {
        val FValue (xs, c, v) = fenv(f)
        val env1 = xs.zip(vs.map(evalVal(env, _))).toMap
        val env2 = exec(fenv, env1, c)
        evalVal(env2, v)
      }
    }

  def goto(c: Code, l: Label): Code =
    c match {
      case Nil => throw(new Error())
      case LabelInstr(l1)::c if (l == l1) => c
      case _::c => goto(c, l)
    }

  def evalCE(env: Env, ce: CExp) : Boolean =
    ce match {
      case EqCExp(v1, v2) => {
        val w1 = evalVal(env, v1)
        val w2 = evalVal(env, v2)
          (w1, w2) match {
          case (IntValue(i1), IntValue(i2)) => i1 == i2
          case _  => throw(new Error())
        }
      }
      case LtCExp(v1, v2) => {
        val w1 = evalVal(env, v1)
        val w2 = evalVal(env, v2)
          (w1, w2) match {
          case (IntValue(i1), IntValue(i2)) => i1 < i2
          case _  => throw(new Error())
        }
      }
      case IsEmptyCExp(v) => {
        val w = evalVal(env, v)
        w match {
          case ListValue(l) => l.isEmpty
          case _  => throw(new Error())
        }
      }
    }

  def exec (fenv: FEnv,  env: Env, c: Code) : Env =
    c match {
      case Nil => env
      case AssignInstr(x, e)::c1 => {
        val env1 = env + (x -> evalExp(fenv, env, e))
        exec(fenv, env1, c1)
      }
      case LabelInstr(l)::c1 => exec(fenv, env, c1)
      case GotoInstr(l)::c1 => exec(fenv, env, goto(c1, l))
      case IfGotoInstr(ce, l)::c1 =>
        if (evalCE(env, ce)) 
          exec(fenv, env, goto(c1, l))
        else
          exec(fenv, env, c1)
    }

  def defs2env (ds: List[Def]): Map[Var, FValue] =
    ds.map(d => (d.name, FValue(d.args, d.code, d.result))).toMap

}
    
