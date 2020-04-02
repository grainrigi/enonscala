package codegen

// コンパイル済みのobject Asmが使われる
// 参考のためオブジェクト名を変更してソースファイルを提供している
object Asm {
  import parsing.Abssyn.Var

  type Reg = Var

  trait Operand {
    override def toString(): String
  }

  case class RegOpd(r: Reg) extends Operand {
    override def toString() = r
  }
  case class IntOpd(i: Int) extends Operand {
    override def toString() = "$"+i.toString
  }

  trait Mem {
    override def toString(): String
    val use: Set[Reg]
  }

  case class MemReg(r: Reg, offset: Int) extends Mem {
    override def toString() = s"$offset($r)"
    val use = Set(r)
  }

  // スピルした変数（フレームに割り当てる前）
  case class MemTmp(s: String) extends Mem { 
    override def toString() = s"Mem[$s]"
    val use = Set[Reg]()
  }

  // スピルした変数（フレームに割り当てた後）
  case class MemBp(i: Int) extends Mem {
    override def toString() =s"${(i+1) * -8}(%rbp)" 
    val use = Set[Reg]()
  }

  val argRegs =
    List("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9")

  val retReg = "%rax"  

  val callerSaveRegs = retReg::argRegs

  val calleeSaveRegs =
    List("%rbx", "%r12", "%r13", "%r14", "%r15")

  val allRegs = callerSaveRegs:::calleeSaveRegs

  val numRegs = allRegs.length

  type Label = String

  trait Instr {
    override def toString(): String
    val use: Set[Reg]
    val defR: Set[Reg]
  }

  def regOfOpd (ri: Operand) : Set[Reg] =
    ri match {
      case RegOpd(r) => Set(r)
      case IntOpd(_) => Set()
    }

  case class Movq (s: Operand, d: Reg) extends Instr {
    override def toString() = s"\tmovq $s,$d\n"
    val use = regOfOpd(s)
    val defR = Set(d)
  }
  case class Loadq (s: Mem, d: Reg) extends Instr {
    override def toString() = s"\tmovq $s,$d\n"
    val use = s.use
    val defR = Set(d)
  }
  case class Storeq (s: Reg, d: Mem) extends Instr {
    override def toString() = s"\tmovq $s,$d\n"
    val use = Set(s)
    val defR = Set[Reg]()
  }
  case class Addq(s: Operand,  d: Reg) extends Instr {
    override def toString() = s"\taddq $s,$d\n"
    val use = regOfOpd(s) + d
    val defR = Set(d)
  }
  case class Subq(s: Operand,  d: Reg) extends Instr {
    override def toString() = s"\tsubq $s,$d\n"
    val use = regOfOpd(s) + d
    val defR = Set(d)
  }
  case class Imulq(s: Operand, d: Reg) extends Instr {
    override def toString() = s"\timulq $s,$d\n"
    val use = regOfOpd(s) + d
    val defR = Set(d)
  }
  case class Callq(s: String, num: Int) extends Instr {
    override def toString() = s"\tcallq _$s\n"
    val use = argRegs.take(num).toSet
    val defR = Set(retReg) ++ argRegs
  }
  case object Retq extends Instr {
    override def toString() = s"\tretq\n"
    val use = Set(retReg)
    val defR = Set[Reg]()
  }
  case class Cmpq(s1: Operand, s2: Reg) extends Instr {
    override def toString() = s"\tcmpq $s1,$s2\n"
    val use = regOfOpd(s1) + s2
    val defR = Set[Reg]()
  }
  case class Pushq (s: Reg) extends Instr {
    override def toString() = s"\tpushq $s\n"
    val use = Set(s)
    val defR = Set[Reg]()
  }
  case class Popq (d: Reg) extends Instr {
    override def toString() = s"\tpopq $d\n"
    val use = Set[Reg]()
    val defR = Set(d)
  }
  case class LabelInstr(l: Label) extends Instr {
    override def toString() = s"$l:\n"
    val use = Set[Reg]()
    val defR = Set[Reg]()
  }
  case class Jmp(l: Label) extends Instr {
    override def toString() = s"\tjmp $l\n"
    val use = Set[Reg]()
    val defR = Set[Reg]()
  }

  trait CC
  case object EqCC extends CC
  case object LtCC extends CC

  case class Jcc(cc: CC, l: Label) extends Instr {
    override def toString() = 
      cc match {
        case EqCC => s"\tje $l\n"
        case LtCC => s"\tjl $l\n"
      }
    val use = Set[Reg]()
    val defR = Set[Reg]()
  }

  case object Cqto extends Instr {
    override def toString() = s"\tcqto\n"
    val use = Set("%rax")
    val defR = Set("%rax", "%rdx")
  }

  case class Idivq (s: Reg) extends Instr {
    override def toString() = s"\tidivq $s\n"
    val use = Set("%rax", "%rdx", s)
    val defR = Set("%rax", "%rdx")
  }

  case class Incq (d: Reg) extends Instr {
    override def toString() = s"\tincq $d\n"
    val use = Set(d)
    val defR = Set(d)
  }

  case class Decq (d: Reg) extends Instr {
    override def toString() = s"\tdecq $d\n"
    val use = Set(d)
    val defR = Set(d)
  }

  type Code = List[Instr]

  case class Def(name: String, numArgs: Int, body: Code)
}

