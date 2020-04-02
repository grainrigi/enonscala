import lex.Yylex
import parsing.Parser
import codegen.IL
import codegen.Emit
import codegen.Codegen
import codegen.Asm
import regalloc.RegAlloc
import regalloc.RegAllocCoalesce

object Main {
    def parseStrDefs(s: String) = {
      val lexer = new Yylex (new java.io.StringReader(s))
      val parser = new Parser(lexer)
      parser.Ds()
    }

    def parseFileDefs(s: String) = {
      val f = new java.io.File(s)
      val lexer = new Yylex (new java.io.FileReader(f))
      val parser = new Parser(lexer)
      parser.Ds()
    }

    def transFileDefs(s: String) = {
      val ds = parseFileDefs(s)
      ds.map(IL.transDef(_))
    }

    def codegenFileDefs0(regAllocDef: Asm.Def => Asm.Def, s: String) = {
      val ds = parseFileDefs(s)
      val ds1 = ds.map(IL.transDef(_))
      val ds2 = ds1.map(Codegen.genDef(_))
      ds2.map(regAllocDef(_))
    }


    def codegenFileDefs(s: String) = 
      codegenFileDefs0(RegAlloc.codegenDef, s)

    def codegenFileDefsCoalesce(s: String) =
      codegenFileDefs0(RegAllocCoalesce.codegenDef, s)    

    def codegenFile(s: String) = {
      val ds = codegenFileDefs(s)
      for (d <- ds) {
        Emit.emitDef(d.name)
        Emit.emitCode(d.body)
        printf("\n")
      }
    }

    var applyCoalesce = true

    def compile(coalesce: Boolean, fname: String): Unit = {
      if (!fname.endsWith(".scala")) throw(new Error())
      val name = fname.dropRight(6)
      val pw = new java.io.PrintWriter(s"misc/$name.s")
      val ds =
        if (coalesce)
          codegenFileDefsCoalesce(s"examples/$fname")
        else
          codegenFileDefs(s"examples/$fname")
      for (d <- ds) {
        Emit.emitDefFile(pw, d.name)
        Emit.emitCodeFile(pw, d.body)
        pw.write("\n")
      }
      pw.close()
    }


    def main(args: Array[String]) = compile(applyCoalesce, args(0))

    val nonscalaFiles =
      List(
        "arith.scala",
        "fact.scala",
        //"introot.scala",
        "sort.scala",
        "qsort.scala",
        "primes.scala",
        //"remainder.scala",
        //"qsortrand.scala"
        )

    def compileAll(coalesce: Boolean) =
      for (f <- nonscalaFiles)
        compile(coalesce, f)


}