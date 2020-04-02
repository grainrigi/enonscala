package typing

import parsing.Abssyn._
import parsing.Oper._
import parsing.Ty._

object TypeCheck {

  case class TypeError(s: String) extends Throwable
  def typeError(s: String) = {throw(TypeError(s))}

  case class FuncTy(ts: List[Ty], result: Ty)

  // 式 e の型を返す
  // 型エラーがあった場合は, 例外 TypeError を発生させる
  // tcheck
  def tcheck (fenv: Map[Var, FuncTy], env: Map[Var, Ty], e: Exp) : Ty =
    e match {
      case VarExp(x) =>
        env.get(x) match {
          case Some(t) => t
          case None => typeError(s"Cannot find variable: $x")
        }
      case UOpExp(o, e1) => {
        val t1 = tcheck (fenv, env, e1)
        (o, t1) match {
          case (IsEmptyOp, IntListTy) => BoolTy
          case (HeadOp, IntListTy) => IntTy
          case (TailOp, IntListTy) => IntListTy
          case _ => typeError(s"Unary Operation $o cannot be applied to $t1")
        }
      }
      case IntExp(i) => IntTy
      case NilExp => IntListTy
      case BOpExp(o, e1, e2) => {
        val t1 = tcheck(fenv, env, e1)
        val t2 = tcheck(fenv, env, e2)
        (o, t1, t2) match {
          case (PlusOp, IntTy, IntTy) => IntTy
          case (MinusOp, IntTy, IntTy) => IntTy
          case (TimesOp, IntTy, IntTy) => IntTy
          case (DivideOp, IntTy, IntTy) => IntTy
          case (EqOp, IntTy, IntTy) => BoolTy
          case (LtOp, IntTy, IntTy) => BoolTy
          case (ConsOp, IntTy, IntListTy) => IntListTy
          case _ => typeError(s"Binary Operation $o cannot be used between $t1 and $t2")
        }
      }
      case IfExp(e, e1, e2) => {
        val t = tcheck(fenv, env, e)
        val t1 = tcheck(fenv, env, e1)
        val t2 = tcheck(fenv, env, e2)
        t match {
          case BoolTy =>
            if(t1 == t2) t1
            else typeError(s"if-else generates multiple types: $t1 and $t2")
          case _ => typeError(s"condition clause is not Boolean")
        }
      }
      case AppExp(f, es) => {
        fenv.get(f) match {
          case Some(FuncTy(ts, t)) => {
            if(es.length != ts.length) typeError(s"arity mismatch for call of $f")
            else es.map(x => tcheck(fenv, env, x)).zip(ts).foldLeft(t)((s, x) => x match {
              case (t1, t2) if t1 == t2 => t
              case (t1, t2) => typeError(s"argument type mismatch for call of $f: expected $t2, got $t1")
            })
          }
          case None => typeError(s"Cannot find function: $f")
        }
      } 
      case BlockExp(es) => {
        es match {
          case Nil => UnitTy // 空のブロックはUnit
          case head::tail => head match {
            case ValExp(x, e) => { // val文
              val newenv = env + (x -> tcheck(fenv, env, e))
              tcheck(fenv, newenv, BlockExp(tail))
            }
            case _ => { // 式文
              val headty = tcheck(fenv, env, head)
              if(tail == Nil) headty // 最後の式の値がブロックの値
              else tcheck(fenv, env, BlockExp(tail))
            }
          }
        }
      }

    }

  def defs2fenv (ds: List[Def]): Map[Var, FuncTy] =
    ds.map(d =>
      (d.name, FuncTy(d.args.map(_._2), d.rtype))).toMap

  // 型エラーがあった場合は, 例外 TypeError を発生させる
  def tcheckDefs (ds: List[Def]): Unit = {
    val fenv = defs2fenv(ds)
    for (d <- ds)
      if (tcheck(fenv, d.args.toMap, d.body) != d.rtype)
        typeError(s"Type error: ${d.name}")

  }
}
