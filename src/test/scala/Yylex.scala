import org.scalatest._
import java.io.StringReader
import lex.Tokens._
import lex.Yylex

class YylexTest extends FlatSpec {
  "10" should "整数" in
  {
    val l = new Yylex(new StringReader("10"))

    assert(l.yylex() == INT(10))
  }

  "0" should "整数" in
  {
    val l = new Yylex(new StringReader("0"))

    assert(l.yylex() == INT(0))
  }

  "識別子" should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("ab0c1d"))

    assert(l.yylex() == ID("ab0c1d"))
  }

  "トークンif" should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("if"))

    assert(l.yylex() == IF)
  }


  "if0" should "識別子" in
  {
    val l = new Yylex(new StringReader("if0"))

    assert(l.yylex() == ID("if0"))
  }

  "val" should "正しい字句解析" in
  {
    val l = new Yylex(new StringReader("val"))

    assert(l.yylex() == VAL)
  }

}
