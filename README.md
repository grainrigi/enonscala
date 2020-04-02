# 行った課題
- Nonscalaをval文で拡張
- ゴミ集めで生存していない変数を根としないようにする

# val文の拡張
## ENonscalaの文法
式ブロック、val文、Unit型が追加されている。
式ブロックは+-*/によって分割されないためFactorである。
```
B -> E == E | E < E | E
C -> E :: C | E
D -> def id ( L ) : U = I
D' -> D D' | D
E -> E + T | E - T | T
F -> Nil | id . head | id . tail | num | id | id ( K ) | id . isEmpty | ( E ) | { S' } ※式ブロックを追加
I -> if ( B ) I else I | C
K -> C , K | C | ε
L -> id : U , L | id : U | ε
S -> D' $ | I $
T -> T * F | T / F | F
U -> Int | Boolean | List[Int] | Unit ※Unit型を追加
S' -> ε | S'' | S'' ; S' ※ブロック式の中身
S'' -> V | I ※文
V -> val id = I ※val文
```

## 式ブロックについて
式ブロックは、複数の文(文はval文または式文のみ)をまとめて一つの式に出来る記法である。
文と文の間はセミコロンで区切らなければならない(MUST)。
```
{
    val x = 3;
    val y = 5;
    x + y
}
```
この式の値は8となる。
即ち、最後に記述された文が式文であればその式の値がブロック式の値となる。
最後がval文であった場合、また空ブロックはUnitとなる。

## ブロックスコープと変数隠蔽
式ブロックは独立したスコープを持ち、外部スコープの変数を隠蔽する。
```
{
    val x = 3;
    {
        val x = 5;
        x // 5
    }
}
```

## 変更した箇所
- {}(LBRACE,RBRACE)および;(SEMICOLON)を入力するためflex/ENonscala.flexを追加した
- ↑の変更がlex.Tokens, lex.Yylexに反映されている
- parsing.AbssynにUnitExp,ValExp,BlockExpを追加した
- parsing.TyにUnitTyを追加した
- parsing.ParserはENonscalaの文法に基づいて書き直されている(解析表はensc.csvにある)
- 変数隠蔽のために、中間コードの変数の名前を変換するcodegen.IL.translateXXVarメソッドを追加した
- codegen.IL.transにBlockExpのケースを追加した。ここではブロック中のValExpの出現に応じて環境を変更しながら再帰的にブロックの文を処理していく。また、ブロックローカルな変数のために変数名の変換も行なう。
- Codegen,ILExec,AsmExecにUnit値を取り扱うコードを追加した
- examples/qsort.sのqsort関数をval文を用いて書き直した


# 静的に生存していない変数をGCの根としない

(以下、スライドの説明の補足)

ゴミがスタックに保存される可能性があるのは、
callee-saveレジスタにゴミを入れたまま関数を呼び出した場合である。
よって、callqの直前に生存していないcallee-saveレジスタの内容を0クリアすれば良い。

また、スタックフレーム確保時にパディングを入れた場合、
ゴミが残っている領域にパディングを入れる可能性があるため
パディングを0クリアする処理を追加する必要がある。

## 変更した箇所
- regalloc.LivenessにlivenessListメソッドを追加した。これはlivenessの計算時に末尾から生存変数を解析した結果を捨てずにリスト化したものである。生成元のcodeとzipすると各instrの実行直前に生存している変数が分かる
- regalloc.RegAlloc.clearDeadArgsメソッドを追加した。これは与えられたcodeに先述の0クリア処理を挿入するもので、↑のlivenessListメソッドを活用する。
- regalloc.RegAlloc.genBodyメソッドにおいて、実レジスタ割当を行ったコードにclearDeadArgsで0クリア処理を挿入した
- regalloc.RegAlloc.genProEpiメソッドにおいてパディングを0クリアする処理を追加した

# 改善の効果
上記2つの改善により、書き直されたqsortは500個までの要素をソートできるようになった。
(ヒープサイズが1000なので、使用領域がO(n)となったと考えられる。)

## 動かし方
```
$ sbt
sbt> console
scala> Main.compileAll(true)
scala> :q
sbt> exit

$ cd misc
$ gcc qsort.s main.c -o qsort
$ ./qsort 500
```

