package lex

object Tokens {

  trait Token
  case class ID(s: String) extends Token
  case class INT(i: Int) extends Token
  case object IF extends Token
  case object ELSE extends Token
  case object DEF extends Token
  case object VAL extends Token
  case object NIL extends Token

  case object LPAREN extends Token
  case object RPAREN extends Token
  case object LBRACKET extends Token
  case object RBRACKET extends Token
  case object LBRACE extends Token
  case object RBRACE extends Token
  case object COMMA extends Token
  case object PLUS extends Token
  case object MINUS extends Token
  case object TIMES extends Token
  case object DIV extends Token
  case object DOT extends Token
  case object SEMICOLON extends Token
  case object COLON extends Token
  case object COLONCOLON extends Token
  case object EQ extends Token
  case object EQEQ extends Token
  case object LESS extends Token
  case object EOF extends Token
}
