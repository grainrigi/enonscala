package parsing.tools

import scala.util.control.Breaks

/* 文法から予測型構文解析表を生成するためのプログラム群 */
// 具体的な文法はGrammers.scalaに記載

// 文法を表現するための記号
trait GrammerSymbol {
  def toSimpleForm() = 
    this match {
      case SSymbol(s) => s
      case VSymbol(c, p) => c + "'" * p
      case EmptySymbol => "ε"
    }
}
case class SSymbol(s: String) extends GrammerSymbol // 終端記号 
case class VSymbol(c: Char, prime: Int = 0) extends GrammerSymbol // 非終端記号
case object EmptySymbol extends GrammerSymbol // 空文字列

// 集合と要素、または集合と集合の関係を表現
trait SetRelation
case class IncludeRelation(outer: VSymbol, inner: VSymbol) extends SetRelation // innerはouterの部分集合である
case class HaveRelation(set: VSymbol, elem: SSymbol) extends SetRelation // elemはsetの元である

object SetRelation {
  // Thunk: 集合の全要素の遅延オブジェクト
  case class Thunk(set: VSymbol) extends GrammerSymbol

  // 集合の関係を解決しそれぞれの集合の要素を外延的に列挙する
  def resolveRelations(rels: Set[SetRelation]): Map[VSymbol, Set[SSymbol]] = {
    var thunkMap = rels.foldLeft(
      Map[VSymbol, Set[GrammerSymbol]]().withDefaultValue(Set())
    )((m, x) => {
      x match {
        case IncludeRelation(o, i) => m + (o -> (m(o) + Thunk(i))) // Thunk(i)を加える
        case HaveRelation(s, e) => m + (s -> (m(s) + e)) // 要素eを加える
      }
    })

    // Thunkを消し終わったものだけをretに入れることとする
    var ret = Map[VSymbol, Set[SSymbol]]()

    // すべてのThunkを消し終わるまで処理を続ける
    while(thunkMap.keys.size > ret.keys.size) {
      // それぞれの集合について処理する
      thunkMap = thunkMap.map{ case (s, es) =>
        val thunks = es.collect{ case t: Thunk => t } // 要素のうちThunkのみ
        if (thunks.size == 0) { // Thunkがなければ処理は完了なのでretに追加する
          ret += (s -> es.collect{ case s: SSymbol => s })
          (s -> es)
        }
        else {
          (s -> thunks.foldLeft(es){ case (es, t) => {
            if(t.set != s) es - t ++ thunkMap(t.set) // よその集合のThunkはその中身で置き換える
            else es - t // 自身のThunkは無意味なので単に消す
          }})
        }
      }.toMap
    }

    ret
  }
}


class Grammer (
  val P: Map[VSymbol, Set[List[GrammerSymbol]]], // 生成規則
  val S: VSymbol) { // 開始記号

  // 曖昧性のない文法に変換
  lazy val toNonAmbiguous: Grammer = toNonLeftRecursive.toLeftFactored

  // 左再帰のない文法に変換
  private lazy val toNonLeftRecursive: Grammer = {
    // 非終端記号の'の数を1増やす
    val nextPrimeVSymbol = (c: Char) => VSymbol(
      c,
      P.keys.collect{ case VSymbol(cc, p) if c == cc => p }.max + 1
    )

    val modP = P.flatMap{ case (x, rule) => {
      // 左再帰になっているものとそうでないものを分類
      val leftRecursive = rule.filter(r => r.head == x)
      if (leftRecursive.size > 0) {
        // 左再帰があるから再構成する
        val nonRecursive = rule.filter(r => r.head != x)
        val altV = nextPrimeVSymbol(x.c)

        List((x -> nonRecursive.map(r => r ++ List(altV))), // 再帰でないものは、生成元は変化せずにX'が後ろに付く
             (altV -> (leftRecursive.map(r => r.tail ++ List(altV)) + List(EmptySymbol)))) // 再帰なものはX'から生成し、右再帰する
      }
      else List((x, rule))
    }}

    new Grammer(modP, S)
  }

  // 左括り出しを行った文法を返す
  // ※左再帰を含む文法を処理すると正しくない結果となる
  private lazy val toLeftFactored: Grammer = {
    val primes = scala.collection.mutable.Map[Char, Int]().withDefault(c => P.keys.collect{ case VSymbol(cc, p) if c == cc => p }.max + 1)
    // 非終端記号の'の数を1増やす
    val nextPrime: Char => Int = c => {
      val p = primes(c)
      primes.put(c, p + 1)
      p
    }

    var modP = P
    var mods = Set[(VSymbol, Set[List[GrammerSymbol]])]()

    // 先頭の共通部分を括りだして分離する
    // 前提: 与えられた集合の各元の先頭は全て同じシンボルである
    lazy val separate: (Set[List[GrammerSymbol]]) => (List[GrammerSymbol], Set[List[GrammerSymbol]]) = rules => {
      var head: GrammerSymbol = rules.head.head
      // 先頭を飛ばす
      var tails = rules.map(r => r.tail)

      // 先頭が一致しなくなっていればこれで完了
      if (tails.contains(Nil) || tails.groupBy(t => t.head).keys.size > 1) (head::Nil, tails)
      // そうでなければ引き続き分離する
      else {
        val res = separate(tails)
        (head::res._1, res._2)
      }
    }

    // 生成規則の左括り出しを行う
    // 戻り値: もとの非終端記号に対する新しい生成規則、および括りだしによって追加された新たな生成規則
    lazy val leftFactor: ((VSymbol, Set[List[GrammerSymbol]])) => Iterable[(VSymbol, Set[List[GrammerSymbol]])] = { case (x, rules) => {
        // 先頭要素によってグルーピング
        val groups = rules.groupBy(r => r.head)
        // 括り出しが必要なグループを抽出
        val groupeds = groups.values.filter(v => v.size >= 2)

        // 括り出しが必要なグループがいなければ元の生成規則そのまま
        if (groupeds.size == 0) Iterable((x -> rules))
        // 順次括りだす
        else {
          // 括りだしの必要がない生成規則たち
          val soles = groups.values.filter(v => v.size == 1).flatten
          // 括りだしが必要な全てのグループで最長括り出しを行う (新しいprime, (括りだされた部分, 後続の部分の集合))
          val separateds = groupeds.map(g => (nextPrime(x.c), separate(g)))
          // 元の非終端記号に追加された生成規則を抽出 (X -> (括り出し部分)X')
          val addeds = separateds.map(s => s._2._1 :+ VSymbol(x.c, s._1))
          // 元の非終端記号に追加された生成規則と、括りだしによって追加された規則(これも再帰的に括り出しを行う)
          Iterable((x -> (soles ++ addeds).toSet)) ++ separateds.flatMap(s => leftFactor(VSymbol(x.c, s._1), s._2._2.map(x => if (x.size == 0) List(EmptySymbol) else x)))
        }
    }}

    new Grammer(P.flatMap(leftFactor), S)
  }


  lazy val firstMap: Map[VSymbol, Set[SSymbol]] = SetRelation.resolveRelations(firstRelations)

  // firstに関する包含関係を導出
  lazy val firstRelations: Set[SetRelation] = {
    // あるruleから導出できる全てのSetRelationを列挙
    // 前提: ruleが規則の一部分の場合、それはnullablesによって先行されているものとする
    lazy val collectRelations: (VSymbol, List[GrammerSymbol]) => Set[SetRelation] = (x, rule) => rule match {
      // X -> aα のパターン
      case (s: SSymbol)::_ => Set(HaveRelation(x, s))
      // X -> (nullables)Yα のパターン
      case (v: VSymbol)::rest =>
        // 現在の非終端記号がnullableならその後ろもnullablesに先行されると言える
        if (nullables.contains(v)) collectRelations(x, rule.tail) + IncludeRelation(x, v)
        else Set(IncludeRelation(x, v))
      case _ => Set()
    }

    P.flatMap{ case (x, rules) => rules.flatMap(r => collectRelations(x, r)) }.toSet
  }

  // γ ∈ ( V ∪ Σ )* に対して拡張されたfirst
  def firstOf(rule: List[GrammerSymbol]): Set[SSymbol] = rule match {
    case (s: SSymbol)::_ => Set(s)
    case (v: VSymbol)::gamma =>
      if (nullables.contains(v)) firstMap(v) ++ firstOf(gamma)
      else firstMap(v)
    case _ => Set()
  }

  lazy val followMap: Map[VSymbol, Set[SSymbol]] = SetRelation.resolveRelations(followRelations)

  lazy val followRelations: Set[SetRelation] = {
    // 生成規則から導出される全てのfirst⊆followの関係を列挙(規則中のすべての非終端記号を走査)
    lazy val collectFollowHaving: List[GrammerSymbol] => Set[HaveRelation] = (rule) => rule match {
      // 非終端記号があれば、後続部分のfirstをfollowに含める
      case (v: VSymbol)::rest =>
        firstOf(rest).map(x => HaveRelation(v, x)) ++ collectFollowHaving(rest)
      // それ以外のときは残りを走査するだけ
      case _::rest => collectFollowHaving(rest)
      case Nil => Set()
    }

    // 生成規則から導出される全てのfollow⊆followの関係を列挙
    val collectFollowInclusion: VSymbol => List[GrammerSymbol] => Set[IncludeRelation] = x => rule => {
      // 前提: ruleの先頭が反転前のリストの真の末尾でないとき、いままで処理したものはすべてnullableである
      lazy val impl: List[GrammerSymbol] => Set[IncludeRelation] = rule => rule match {
        // 非終端記号なら、それはnullableが後続するものである(前提より)
        case (v: VSymbol)::rest =>
          // 自身が消去可能ならば、さらにその後ろの関係も導出される可能性がある
          if (nullables.contains(v)) impl(rest) + IncludeRelation(v, x)
          else Set(IncludeRelation(v,x))
        // 非終端記号以外はnullableとならないから終了
        case _ => Set()
      }

      // 後ろから調べるために反転する(ある時点でnullableでないものが出現したらそこで終了できるため)
      impl(rule.reverse)
    }

    P.flatMap{ case (x, rules) => {
      val inclusions = collectFollowInclusion(x)
      rules.flatMap(r => collectFollowHaving(r) ++ inclusions(r))
    }}.toSet
  }

  // 消去可能な非終端記号の集合
  lazy val nullables: Set[VSymbol] = {
    // setsをnullableの集合として、それらの要素を生成規則(rules)から消去する
    val eliminateNullables = (sets: Set[VSymbol], rules: Set[List[GrammerSymbol]]) =>
      rules.map(r => r.filter{
        case s: VSymbol => !sets.contains(s)
        case _ => true
      })

    // ret: 消去可能な非終端記号であることが確定したものたち 直接εに行くものをまずは集める
    var ret: Set[VSymbol] = P.collect{ case (s, rules) if rules.contains(List(EmptySymbol)) => s }.toSet
    // modP: 消去可能なものをできるだけ消していった規則 現時点で分かっているものは消しておく
    var modP: Map[VSymbol, Set[List[GrammerSymbol]]] = P.collect{ case (x, rules) if !ret.contains(x) => (x -> eliminateNullables(ret, rules)) }.toMap

    val b = new Breaks
    b.breakable {
      // ループ先頭での前提条件: modPにはretの要素は一切含まれない
      while(true) {
        // 新たに消去可能となったもの(空リストを生成する非終端記号)を収集
        val newNullables = modP.collect{ case (x, rules) if rules.contains(Nil) => x }.toSet
        // 消去可能な非終端記号が増えなければ終了
        if (newNullables.size == 0) b.break
        // 空リストを除去してretとmodPを更新(次回の前提条件を満たすため)
        modP = modP.collect{ case (s, rules) if !newNullables.contains(s) => (s -> eliminateNullables(newNullables, rules.filter(x => x != Nil))) }.toMap
        ret = ret ++ newNullables
      }
    }

    ret
  }

  // γ ∈ ( V ∪ Σ )* に対して拡張されたnullable
  def isNullable(rule: List[GrammerSymbol]): Boolean = rule match {
    case Nil => true
    case (v: VSymbol)::rest => nullables.contains(v) && isNullable(rest)
    case EmptySymbol::_ => true
    case _ => false
  }

  // 予測型構文解析表に変換する
  lazy val toPredictiveParsingTable: Map[(VSymbol, SSymbol), Set[List[GrammerSymbol]]] = {
    // 各規則のfirstOfから導出される表の要素
    val fromFirst: Iterable[((VSymbol, SSymbol), List[GrammerSymbol])] =
      P.toIterable.flatMap{ case (x, rules) => {
        rules.flatMap(r => firstOf(r).map(sym => ((x, sym) -> r)))
      }}

    // nullableな非終端記号のfollowから導出される表の要素
    val fromFollow: Iterable[((VSymbol, SSymbol), List[GrammerSymbol])] =
      P.toIterable.flatMap{ case (x, rules) => {
        rules.collect{
          case r: List[GrammerSymbol] if isNullable(r) =>
            // まきちらす
            followMap(x).map(sym => ((x, sym) -> r))
        }.flatten
      }}

    // 全てマージする
    fromFollow.foldLeft(
      fromFirst.foldLeft(Map[(VSymbol, SSymbol), Set[List[GrammerSymbol]]]().withDefaultValue(Set()))
      ((m, x) => m + (x._1 -> (m(x._1) + x._2)))
    )((m, x) => m + (x._1 -> (m(x._1) + x._2)))
  }

  override def toString() = {
    P.toList.sortWith((a, b) => a._1.c < b._1.c || (a._1.c == b._1.c && a._1.prime < b._1.prime)).map{ case (x, rule) => {
      x.toSimpleForm + " -> " + rule.map(GrammerUtil.ruleToString).mkString(" | ")
    }}.mkString("\n")
  }
}

object Grammer {

}

object GrammerUtil {
  // 生成規則の右辺を文字列に変換する
  def ruleToString(rs: List[GrammerSymbol]): String = { rs.map(r => r.toSimpleForm).mkString(" ") }

  // 予測型構文解析表をCSVに変換する
  def pptToCSV(ppt: Map[(VSymbol, SSymbol), Set[List[GrammerSymbol]]]): String = {
    val Vs = ppt.keys.map(x => x._1).toSet.toList.sortWith((a, b) => a.c < b.c || (a.c == b.c && a.prime < b.prime))
    val Ss = ppt.keys.map(x => x._2).toSet.toList.sortWith((a, b) => a.s < b.s)

    val b = new StringBuilder

    b.append("\"\",")
    Ss.map(s => {
      b.append("\"")
      b.append(s.toSimpleForm)
      b.append("\",")
    })
    b.append("\n")

    Vs.map(v => {
      b.append("\"")
      b.append(v.toSimpleForm)
      b.append("\",")
      Ss.map(s => {
        b.append("\"")
        b.append(ppt((v, s)).map(r => v.toSimpleForm + " -> " + ruleToString(r)).mkString("\n"))
        b.append("\",")
        ()
      })
      b.append("\n")
      ()
    })

    b.result
  }
}
