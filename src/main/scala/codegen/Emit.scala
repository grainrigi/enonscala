package codegen

object Emit {
  import Asm._

  def emitDef(fname: String): Unit = {
    printf("\t.globl _%s\n", fname)
    printf("\t.align	4, 0x90\n")
    printf("_%s:\n", fname)
  }

  def emitInstr(i: Instr): Unit =
    i match {
      case Movq(RegOpd(s), d) => if (s != d) printf("\tmovq %s,%s\n", s, d)      
      case Movq(s, d) => printf("\tmovq %s,%s\n", s, d)
      case Loadq (s, d) => printf("\tmovq %s,%s\n", s, d)
      case Storeq (s, d) => printf("\tmovq %s,%s\n", s, d)
      case Addq(s,  d) => printf("\taddq %s,%s\n", s, d)
      case Subq(s,  d) => printf("\tsubq %s,%s\n", s, d)
      case Imulq(s, d) => printf("\timulq %s,%s\n", s, d)
      case Callq(f, _) => printf("\tcallq _%s\n", f)
      case Retq => printf("\tretq\n")
      case Cmpq(s1, s2) => printf("\tcmpq %s,%s\n", s1, s2)
      case Pushq(s) => printf("\tpushq %s\n", s)
      case Popq(d) => printf("\tpopq %s\n", d)
      case LabelInstr(l) => printf("%s:\n", l)
      case Jmp(l) =>  printf("\tjmp %s\n", l)
      case Jcc(EqCC,l) =>  printf("\tje %s\n", l)
      case Jcc(LtCC,l) =>  printf("\tjl %s\n", l)
      case Incq(d) =>  printf("\tincq %s\n", d)
      case Decq(d) =>  printf("\tdecq %s\n", d)
      case Cqto =>  printf("\tcqto\n")
      case Idivq(s) =>  printf("\tidivq %s\n", s)
    }

  def emitCode(c: Code): Unit =
    for (i <- c)
      Emit.emitInstr(i)

  def emitCodeFile(pw: java.io.PrintWriter, c: Code): Unit = {
    for (i <- c)
      i match {
        case Movq(RegOpd(s), d) if (s == d) => ()
        case _ => pw.write(i.toString())
      }
  }

  def emitDefFile(pw: java.io.PrintWriter, fname: String): Unit = {
    pw.write(s"\t.globl _$fname\n")
    pw.write("\t.align	4\n")
    pw.write(s"_$fname:\n")
  }

}
