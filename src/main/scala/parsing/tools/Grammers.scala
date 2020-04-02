package parsing.tools

import scala.collection.immutable.Stream.Empty

/* 問題に提示された文法 (Grammer.scalaに基づく表現) */
// printPPTによって出力された表はcsvディレクトリに入っています

/* DumpableGrammerオブジェクトの使い方
1. 文法の生成規則を出力
toStringによって出力される。
例:
sbt(parsing-sbt)> Grammer1
res6: Grammer1.type =
B -> E == E | E < E
C -> E :: C | E
E -> E + T | E - T | T
F -> id | num | ( E ) | Nil
I -> if ( B ) I else I | C
S -> I $
T -> T * F | T / F | F

2. 文法のfirst, followなどを出力
dumpによって出力される。(lihaoyi.pprintが必要, 同梱のbuild.sbtに置き換えることでインストールされる)
例:
sbt(parsing-sbt)> Grammer1.dump
... (曖昧性のない文法,消去可能記号,first,followが出力される)

3. 文法の予測型構文解析表を出力
printPPTによって出力される。
例: 
sbt(parsing-sbt)> Grammer1.printPPT
... (CSV形式の表が出力される)

※ 出力された表のうち、=で始まるセルはソフトによってエラーになることがある(==など)
 */


trait DumpableGrammer extends Grammer {
  def dump() = {
    println("for Grammer:")
    println(this)
    println("nonAmbiguous:")
    val nl = toNonAmbiguous
    println(nl)
    println("nullables:")
    println(nl.nullables)
    println("first:")
    pprint.pprintln(nl.firstMap)
    println("follow:")
    pprint.pprintln(nl.followMap)
  }

  // 曖昧性のない予測型構文解析表をCSVとして出力する
  def printPPT() = println(GrammerUtil.pptToCSV(toNonAmbiguous.toPredictiveParsingTable))
}

// 必須課題の文法
object Grammer1 extends Grammer(
    Map(
      (VSymbol('F') -> Set(
        List(SSymbol("id")),
        List(SSymbol("num")),
        List(SSymbol("("), VSymbol('E'), SSymbol(")")),
        List(SSymbol("Nil")),
      )),
      (VSymbol('T') -> Set(
        List(VSymbol('T'), SSymbol("*"), VSymbol('F')),
        List(VSymbol('T'), SSymbol("/"), VSymbol('F')),
        List(VSymbol('F')),
      )),
      (VSymbol('E') -> Set(
        List(VSymbol('E'), SSymbol("+"), VSymbol('T')),
        List(VSymbol('E'), SSymbol("-"), VSymbol('T')),
        List(VSymbol('T')),
      )),
      (VSymbol('B') -> Set(
        List(VSymbol('E'), SSymbol("=="), VSymbol('E')),
        List(VSymbol('E'), SSymbol("<"), VSymbol('E')),
      )),
      (VSymbol('C') -> Set(
        List(VSymbol('E'), SSymbol("::"), VSymbol('C')),
        List(VSymbol('E')),
      )),
      (VSymbol('I') -> Set(
        List(SSymbol("if"), SSymbol("("), VSymbol('B'), SSymbol(")"), VSymbol('I'),
             SSymbol("else"), VSymbol('I')),
        List(VSymbol('C')),
      )),
      (VSymbol('S') -> Set(
        List(VSymbol('I'), SSymbol("$")),
      )),
    )
    , VSymbol('S')
  ) with DumpableGrammer

object GrammerTest extends Grammer (
    Map(
      (VSymbol('S') -> Set(
        List(VSymbol('A'), VSymbol('B'), SSymbol("$")),
      )),
      (VSymbol('A') -> Set(
        List(EmptySymbol),
        List(VSymbol('D'), SSymbol("b")),
      )),
      (VSymbol('B') -> Set(
        List(VSymbol('D')),
        List(SSymbol("a"), VSymbol('D'), VSymbol('C')),
      )),
      (VSymbol('C') -> Set(
        List(SSymbol("f"), VSymbol('B')),
        List(SSymbol("b"), VSymbol('C'), SSymbol("a")),
      )),
      (VSymbol('D') -> Set(
        List(EmptySymbol),
        List(SSymbol("d")),
      )),
    )
    , VSymbol('S')
  ) with DumpableGrammer
  
// オプション課題の文法
object GrammerNonscala extends Grammer (
    Map(
      (VSymbol('F') -> Set(
        List(SSymbol("id")),
        List(SSymbol("num")),
        List(SSymbol("Nil")),
        List(SSymbol("("), VSymbol('E'), SSymbol(")")),
        List(SSymbol("id"), SSymbol("("), VSymbol('K'), SSymbol(")")),
        List(SSymbol("id"), SSymbol("."), SSymbol("isEmpty")),
        List(SSymbol("id"), SSymbol("."), SSymbol("head")),
        List(SSymbol("id"), SSymbol("."), SSymbol("tail")),
      )),
      (VSymbol('K') -> Set(
        List(VSymbol('C'), SSymbol(","), VSymbol('K')),
        List(VSymbol('C')),
        List(EmptySymbol),
      )),
      (VSymbol('T') -> Set(
        List(VSymbol('T'), SSymbol("*"), VSymbol('F')),
        List(VSymbol('T'), SSymbol("/"), VSymbol('F')),
        List(VSymbol('F')),
      )),
      (VSymbol('E') -> Set(
        List(VSymbol('E'), SSymbol("+"), VSymbol('T')),
        List(VSymbol('E'), SSymbol("-"), VSymbol('T')),
        List(VSymbol('T')),
      )),
      (VSymbol('B') -> Set(
        List(VSymbol('E'), SSymbol("=="), VSymbol('E')),
        List(VSymbol('E'), SSymbol("<"), VSymbol('E')),
        List(VSymbol('E')),
      )),
      (VSymbol('C') -> Set(
        List(VSymbol('E'), SSymbol("::"), VSymbol('C')),
        List(VSymbol('E')),
      )),
      (VSymbol('I') -> Set(
        List(SSymbol("if"), SSymbol("("), VSymbol('B'), SSymbol(")"), VSymbol('I'),
             SSymbol("else"), VSymbol('I')),
        List(VSymbol('C')),
      )),
      (VSymbol('D') -> Set(
        List(SSymbol("def"), SSymbol("id"), SSymbol("("), VSymbol('L'), SSymbol(")"), SSymbol(":"), VSymbol('U'), SSymbol("="), VSymbol('I')),
      )),
      (VSymbol('L') -> Set(
        List(SSymbol("id"), SSymbol(":"), VSymbol('U'), SSymbol(","), VSymbol('L')),
        List(SSymbol("id"), SSymbol(":"), VSymbol('U')),
        List(EmptySymbol),
      )),
      (VSymbol('U') -> Set(
        List(SSymbol("Int")),
        List(SSymbol("Boolean")),
        List(SSymbol("List[Int]")), // 表が冗長になるので全ての記号をひとつにまとめた
      )),
      (VSymbol('D', 1) -> Set( // Dsと同じ
        List(VSymbol('D'), VSymbol('D', 1)),
        List(VSymbol('D')),
      )),
      (VSymbol('S') -> Set(
        List(VSymbol('D', 1), SSymbol("$")),
        List(VSymbol('I'), SSymbol("$")),
      )),
    )
    , VSymbol('S')
  ) with DumpableGrammer

// ENonscalaの文法
object GrammerENonscala extends Grammer (
    Map(
      (VSymbol('F') -> Set(
        List(SSymbol("id")),
        List(SSymbol("num")),
        List(SSymbol("Nil")),
        List(SSymbol("("), VSymbol('E'), SSymbol(")")),
        List(SSymbol("{"), VSymbol('S', 1), SSymbol("}")),
        List(SSymbol("id"), SSymbol("("), VSymbol('K'), SSymbol(")")),
        List(SSymbol("id"), SSymbol("."), SSymbol("isEmpty")),
        List(SSymbol("id"), SSymbol("."), SSymbol("head")),
        List(SSymbol("id"), SSymbol("."), SSymbol("tail")),
      )),
      (VSymbol('K') -> Set(
        List(VSymbol('C'), SSymbol(","), VSymbol('K')),
        List(VSymbol('C')),
        List(EmptySymbol),
      )),
      (VSymbol('T') -> Set(
        List(VSymbol('T'), SSymbol("*"), VSymbol('F')),
        List(VSymbol('T'), SSymbol("/"), VSymbol('F')),
        List(VSymbol('F')),
      )),
      (VSymbol('E') -> Set(
        List(VSymbol('E'), SSymbol("+"), VSymbol('T')),
        List(VSymbol('E'), SSymbol("-"), VSymbol('T')),
        List(VSymbol('T')),
      )),
      (VSymbol('B') -> Set(
        List(VSymbol('E'), SSymbol("=="), VSymbol('E')),
        List(VSymbol('E'), SSymbol("<"), VSymbol('E')),
        List(VSymbol('E')),
      )),
      (VSymbol('C') -> Set(
        List(VSymbol('E'), SSymbol("::"), VSymbol('C')),
        List(VSymbol('E')),
      )),
      (VSymbol('I') -> Set(
        List(SSymbol("if"), SSymbol("("), VSymbol('B'), SSymbol(")"), VSymbol('I'),
             SSymbol("else"), VSymbol('I')),
        List(VSymbol('C')),
      )),
      (VSymbol('D') -> Set(
        List(SSymbol("def"), SSymbol("id"), SSymbol("("), VSymbol('L'), SSymbol(")"), SSymbol(":"), VSymbol('U'), SSymbol("="), VSymbol('I')),
      )),
      (VSymbol('L') -> Set(
        List(SSymbol("id"), SSymbol(":"), VSymbol('U'), SSymbol(","), VSymbol('L')),
        List(SSymbol("id"), SSymbol(":"), VSymbol('U')),
        List(EmptySymbol),
      )),
      (VSymbol('U') -> Set(
        List(SSymbol("Int")),
        List(SSymbol("Boolean")),
        List(SSymbol("List[Int]")), // 表が冗長になるので全ての記号をひとつにまとめた
        List(SSymbol("Unit")),
      )),
      (VSymbol('D', 1) -> Set( // Dsと同じ
        List(VSymbol('D'), VSymbol('D', 1)),
        List(VSymbol('D')),
      )),
      (VSymbol('S') -> Set(
        List(VSymbol('D', 1), SSymbol("$")),
        List(VSymbol('I'), SSymbol("$")),
      )),
      (VSymbol('S', 1) -> Set(
        List(EmptySymbol),
        List(VSymbol('S', 2)),
        List(VSymbol('S', 2), SSymbol(";"), VSymbol('S', 1)),
      )),
      (VSymbol('S', 2) -> Set(
        List(VSymbol('V')),
        List(VSymbol('I')),
      )),
      (VSymbol('V') -> Set(
        List(SSymbol("val"), SSymbol("id"), SSymbol("="), VSymbol('I')),
      )),
    )
    , VSymbol('S')
  ) with DumpableGrammer
