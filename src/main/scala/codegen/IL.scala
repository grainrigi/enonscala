package codegen

import CodegenBase._
import parsing.Abssyn
import parsing.Abssyn.Var
import parsing.Oper._

object IL {

  trait Val
  case class VarVal(s: Var) extends Val
  case class IntVal(i: Int) extends Val
  case object NilVal extends Val
  case object UnitVal extends Val

  trait Exp
  case class ValExp(v: Val) extends Exp
  case class BOpExp(o: BOp, v1: Val, v2: Val) extends Exp
  case class UOpExp(o: UOp, v: Val) extends Exp
  case class CallExp(f: Var, vs: List[Val]) extends Exp

  trait CExp // 比較式
  case class IsEmptyCExp(v: Val) extends CExp
  case class EqCExp(v1: Val, v2: Val) extends CExp
  case class LtCExp(v1: Val, v2: Val) extends CExp

  sealed trait Instr
  case class AssignInstr(x: Var, e: Exp) extends Instr
  case class LabelInstr(l: Label) extends Instr
  case class GotoInstr(l: Label) extends Instr
  case class IfGotoInstr(c: CExp, l: Label) extends Instr

  type Code = List[Instr]

  case class Def(name: String, args: List[Var], code: Code, result: Val)

  // ローカル変数の隠蔽のために変数名の変換を行なう
  def translateCodeVar(code: Code, src: Var, dst: Var): Code = {
    val trans = (x: Var) => if(x == src) dst else x
    code match {
      case Nil => Nil
      case i::is => (i match {
        case AssignInstr(x, e) => AssignInstr(trans(x), translateExpVar(e, src, dst))
        case IfGotoInstr(c, l) => IfGotoInstr(translateCExpVar(c, src, dst), l)
        case _ => i
      }) :: translateCodeVar(is, src, dst)
    }
  }

  def translateExpVar(e: Exp, src: Var, dst: Var): Exp = { 
    val trans = (x: Val) => (x match {
        case VarVal(x) => if(x == src) VarVal(dst) else VarVal(x)
        case _ => x
    }): Val
    e match {
        case ValExp(v) => ValExp(trans(v))
        case BOpExp(o, v1, v2) => BOpExp(o, trans(v1), trans(v2))
        case UOpExp(o, v) => UOpExp(o, trans(v))
        case CallExp(f, vs) => CallExp(f, vs.map(trans))
    }
  }

  def translateCExpVar(c: CExp, src: Var, dst: Var): CExp = {
    val trans = (x: Val) => (x match {
        case VarVal(x) => if(x == src) VarVal(dst) else VarVal(x)
        case _ => x
    }): Val
    c match {
        case IsEmptyCExp(v) => IsEmptyCExp(trans(v))
        case EqCExp(v1, v2) => EqCExp(trans(v1), trans(v2))
        case LtCExp(v1, v2) => LtCExp(trans(v1), trans(v2))
    }
  }


  def trans(e: Abssyn.Exp) : (Code, Val) = 
    e match {
      case Abssyn.VarExp(x) => (Nil, VarVal(x))
      case Abssyn.IntExp(i) => (Nil, IntVal(i))
      case Abssyn.NilExp =>    (Nil, NilVal) 
      case Abssyn.BOpExp(o, e1, e2) => {
        val x = freshVar()
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c1 ++ c2 ++ List(AssignInstr(x, BOpExp(o, v1, v2))),
          VarVal(x))
      }
      case Abssyn.UOpExp(o, e1) => {
        val x = freshVar()
        val (c1, v1) = trans(e1)
        (c1 ++ List(AssignInstr(x, UOpExp(o, v1))), VarVal(x))
      }
      case Abssyn.IfExp(e, e1, e2) => {
        val r = freshVar()
        val thenLabel = freshLabel("then") // thenから始まる新しいラベルを生成
        val nextLabel = freshLabel("next")
        val (c, ce) = transCmp(e)

        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)

        (c ++ List(IfGotoInstr(ce, thenLabel))
           ++ c2 ++ List(AssignInstr(r, ValExp(v2)), GotoInstr(nextLabel),
           LabelInstr(thenLabel))
           ++ c1 ++ List(AssignInstr(r, ValExp(v1)),
           LabelInstr(nextLabel)),
         VarVal(r))
      }
      case Abssyn.AppExp(f, es) => {
        val r = freshVar()
        val (cs, vs) = es.map(x => trans(x)).unzip
        
        (cs.flatten ++ List(AssignInstr(r, CallExp(f, vs))), VarVal(r))
      }
      case Abssyn.BlockExp(es) => es match {
          case Nil => (Nil, UnitVal)
          case head::tail => head match {
            case Abssyn.ValExp(x, e) => { // val文
              val nx = freshVar()
              val transl = (v: Val) => (v match {
                  case VarVal(v) => if(v == x) VarVal(nx) else VarVal(v)
                  case _ => v
              }): Val;
              val (c, v) = trans(e)
              val (cs, vs) = trans(Abssyn.BlockExp(tail))
              val nc = c ++ (AssignInstr(x, ValExp(v)) :: cs)
              (translateCodeVar(nc, x, nx), transl(vs))
            }
            case _ => { // 式文
              val (c, v) = trans(head)
              if(tail == Nil) {
                  (c, v)
              }
              else {
                val (cs, vs) = trans(Abssyn.BlockExp(tail))
                (c ++ cs, vs)
              }
            }
          }
      }
    }

  // if 文の条件部分eを翻訳
  def transCmp(e: Abssyn.Exp) : (Code, CExp) =
    e match {
      case Abssyn.BOpExp(EqOp, e1, e2) => {
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c1 ++ c2, EqCExp(v1,v2))
      }
      case Abssyn.BOpExp(LtOp, e1, e2) => {
        val (c1, v1) = trans(e1)
        val (c2, v2) = trans(e2)
        (c1 ++ c2, LtCExp(v1,v2))
      }
      case Abssyn.UOpExp(IsEmptyOp, e1) => {
        val (c1, v1) = trans(e1)
        (c1, IsEmptyCExp(v1))
      }
      case _ => throw(new Error())
    }

  def transDef(d: Abssyn.Def) : Def = {
    val (c, v) =  trans(d.body)
    Def(d.name, d.args.map(_._1), c, v)
  }

}
