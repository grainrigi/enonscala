package parsing

import Oper._
import Ty._
import Abssyn._
import lex.Tokens._
import lex.Yylex

class Parser (val src: Yylex) {

  var tok: Token = src.yylex()

  class ParseError(s: String) extends Throwable
  def error(s: String = "") = throw(new ParseError(s))

  def advance () =  tok = src.yylex()

  def eat (t: Token) = 
    if (tok == t) advance() else derror(String.format("Expected %s but got %s", tokenToStr(t), tokenToStr(tok, true)))

  def tokenToStr(t: Token, concrete: Boolean = false) = t match {
    case ID(s) if concrete => "identifier '" + s + "'"
    case ID(_) if !concrete => "identifier"
    case INT(n) if concrete => "integer '" + n.toString + "'"
    case INT(_) if !concrete => "integer"
    case IF => "'if'"
    case ELSE => "'else'"
    case DEF => "'def'"
    case VAL => "'val'"
    case NIL => "'nil'"

    case LPAREN => "'('"
    case RPAREN => "')'"
    case LBRACKET => "'['"
    case RBRACKET => "']'"
    case COMMA => "','"
    case PLUS => "'+'"
    case MINUS => "'-'"
    case TIMES => "'*'"
    case DIV => "'/'"
    case DOT => "'.'"
    case SEMICOLON => "';'"
    case COLON => "':'"
    case COLONCOLON => "'::'"
    case EQ => "'='"
    case EQEQ => "'=='"
    case LESS => "'<'"
    case EOF => "EOF"
  }

  // エラーの原因を出力する
  def derror(msg: String = "") = {
    printf("Error: %s\n", if (msg.length > 0) msg else "Unexpected " + tokenToStr(tok, true))
    error()
  }


  def F(): Exp =    // 拡張が必要
    tok match {
      case NIL => advance(); NilExp
      case INT(i) => advance(); IntExp(i)
      case ID(s) => advance(); Fprime(s)
      case LPAREN =>
        {
          eat(LPAREN)
          val e = E()
          eat(RPAREN)
          e
        }
      case LBRACE => 
        {
          eat(LBRACE)
          val ss = BlockExp(Ss())
          eat(RBRACE)
          ss
        }
      case _ => derror()
    }

  def Fprime(s: Var): Exp =
    tok match {
      case LPAREN => {
        eat(LPAREN)
        val args = K()
        eat(RPAREN)
        AppExp(s, args)
      }
      case DOT => eat(DOT); Fprimeprime(s)
      case EOF | RPAREN | TIMES | PLUS | COMMA | MINUS | DIV | COLONCOLON | SEMICOLON | LESS | EQEQ | DEF | ELSE | RBRACE => VarExp(s)
      case _ => derror()
    }

  def Fprimeprime(s: Var): Exp =
    tok match {
      case ID(f) => advance(); f match {
        case "head" => UOpExp(HeadOp, VarExp(s))
        case "isEmpty" => UOpExp(IsEmptyOp, VarExp(s))
        case "tail" => UOpExp(TailOp, VarExp(s))
        case _ => derror()
      }
      case _ => derror()
    }

  def K(): List[Exp] = // 関数適用の引数リスト
    tok match {
      case LPAREN | NIL | ID(_) | INT(_) | LBRACE => Kprime(C())
      case RPAREN => Nil
      case _ => derror()
    }

  def Kprime(e: Exp): List[Exp] =
    tok match {
      case COMMA => eat(COMMA); e::K()
      case RPAREN => e::Nil
      case _ => derror()
    }

  def T(): Exp =
    tok match {
      case ID(_) | INT(_) | LPAREN | NIL | LBRACE =>  Tprime(F())
      case _ => derror()
    }

  def Tprime(e: Exp): Exp =
    tok match {
      case TIMES => eat(TIMES); Tprime(BOpExp(TimesOp, e, F()))
      case DIV => eat(DIV); Tprime(BOpExp(DivideOp, e, F()))
      case EOF | RPAREN | PLUS | COMMA | MINUS | COLONCOLON | SEMICOLON | LESS | EQEQ | DEF | ELSE | RBRACE => e
           // 上のfollow(T')は文法が拡張されたので計算し直す必要がある
      case _ => derror()
    }

  def E(): Exp = // プログラムを書く．補助関数も必要
    tok match {
      case LPAREN | NIL | ID(_) | INT(_) | LBRACE => Eprime(T())
      case _ => derror()
    }

  def Eprime(e: Exp): Exp =
    tok match {
      case PLUS => eat(PLUS); Eprime(BOpExp(PlusOp, e, T()))
      case MINUS => eat(MINUS); Eprime(BOpExp(MinusOp, e, T()))
      case EOF | RPAREN | COMMA | COLONCOLON | SEMICOLON | LESS | EQEQ | DEF | ELSE | RBRACE => e
      case _ => derror()
    }

  def C(): Exp =  // プログラムを書く．補助関数も必要
    tok match {
      case LPAREN | NIL | ID(_) | INT(_) | LBRACE => Cprime(E())
      case _ => derror()
    }

  def Cprime(e: Exp): Exp =
    tok match {
      case COLONCOLON => eat(COLONCOLON); BOpExp(ConsOp, e, C())
      case EOF | RPAREN | COMMA | SEMICOLON | DEF | ELSE | RBRACE => e
      case _ => derror()
    }

  def B(): Exp = // プログラムを書く．補助関数も必要
    tok match {
      case LPAREN | NIL | ID(_) | INT(_) | LBRACE => Bprime(E())
      case _ => derror()
    }

  def Bprime(e: Exp): Exp =
    tok match {
      case LESS => eat(LESS); BOpExp(LtOp, e, E())
      case EQEQ => eat(EQEQ); BOpExp(EqOp, e, E())
      case RPAREN => e
      case _ => derror()
    }

  def I(): Exp = // プログラムを書く．
    tok match {
      case LPAREN | NIL | ID(_) | INT(_) | LBRACE => C()
      case IF => {
        eat(IF)
        eat(LPAREN)
        val cond = B()
        eat(RPAREN)
        val i1 = I()
        eat(ELSE)
        val i2 = I()
        IfExp(cond, i1, i2)
      }
      case _ => derror()
    }

  def D(): Def = // プログラムを書く．
    tok match {
      case DEF => {
        eat(DEF)
        val name: Var = tok match { case ID(s) => advance(); s; case _ => derror() }
        eat(LPAREN)
        val args = L()
        eat(RPAREN)
        eat(COLON)
        val t = U()
        eat(EQ)
        Def(name, args, t, I())
      }
      case _ => derror()
    }

  def L(): List[(Var, Ty)] = // 関数定義の引数リスト
    tok match {
      case ID(s) => {
        advance()
        eat(COLON)
        val t = U()
        Lprime((s, t))
      }
      case RPAREN => Nil
      case _ => derror()
    }

  def Lprime(arg: (Var, Ty)): List[(Var, Ty)] =
    tok match {
      case COMMA => eat(COMMA); arg::L()
      case RPAREN => arg::Nil
      case _ => derror()
    }

  def U(): Ty =
    tok match {
      case ID(t) => advance(); t match {
        case "Boolean" =>  BoolTy
        case "Int" =>  IntTy
        case "List" => {
          eat(LBRACKET)
          eat(ID("Int"))
          eat(RBRACKET)
          IntListTy
        }
        case "Unit" => UnitTy
      }
      case _ => derror()
    }

  def Ds(): List[Def] = {
    tok match {
      case DEF => {
        val d = D()
        val ds = Ds()
        d::ds
      }
      case EOF => Nil
    }
  }

  def Ss(): List[Exp] = {
    tok match {
      case LPAREN | NIL | ID(_) | IF | INT(_) | VAL | LBRACE =>
        Stmt() :: Ssprime()
      case RBRACE => Nil
      case _ => derror()
    }
  }

  def Ssprime(): List[Exp] = {
    tok match {
      case SEMICOLON => {
        eat(SEMICOLON)
        Ss()
      }
      case RBRACE => Nil
      case _ => derror()
    }
  }

  def Stmt(): Exp = {
    tok match {
      case LPAREN | NIL | ID(_) | IF | INT(_) | LBRACE =>
        I()
      case VAL => V()
      case _ => derror()
    }
  }

  def V(): Exp = {
    tok match {
      case VAL => {
        eat(VAL)
        val id = tok match {
          case ID(x) => advance(); x
          case _ => derror()
        }
        eat(EQ)
        ValExp(id, I())
      }
    }
  }
}

