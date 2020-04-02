package codegen

object AsmExec {
  import parsing.Abssyn.Var
  import CodegenBase._
  import Asm._

  type Env = Map[Reg, Int]
  type Mem = Map[Int, Int]

  def evalOperand (env: Env, o: Operand) : Int = 
    o match {
      case RegOpd(r) => env(r)
      case IntOpd(i) => i
    }

  def updateFlag (env: Env, n: Int): Env = {
    val env1 = env + ("%ZF" -> (if (n == 0) 1 else 0))
    env1 + ("%SF" -> (if (n < 0) 1 else 0))
  }

  def execInstr (env: Env, i: Instr):  Env =
    i match {
      case Movq(s, d) => env + (d -> evalOperand(env, s))
      case Addq(s,  d) => {
        val n = env(d) + evalOperand(env, s)
        updateFlag(env, n)  + (d -> n)
      }
      case Subq(s,  d) => {
        val n = env(d) - evalOperand(env, s)
        updateFlag(env, n)  + (d -> n)
      }
      case Imulq(s, d) => {
        val n = env(d) * evalOperand(env, s)
        updateFlag(env, n)  + (d -> n)
      }
      case Cmpq(s1, s2) => {
        val n = env(s2) - evalOperand(env, s1)
        updateFlag(env, n) 
      }
      case Idivq(s) => {
        if (!
          ((env("%rdx") == 0 && env("%rax") >= 0)
            || (env("%rdx") == -1 && env("%rax") < 0)))
          notSupported("idivq")
        val raxn = env("%rax")
        val sn = env(s)
        val env1 = env + ("%rdx" -> raxn % sn)
        env1 + ("%rax" -> raxn / sn)
      }
      case Cqto => {
        val d = if (env("%rax") >= 0) 0 else -1
        env + ("%rdx" -> d)
      }
      case Incq(d) => env + (d -> (env(d) + 1))
      case Decq(d) => env + (d -> (env(d) - 1))
      case LabelInstr(_) => env
      case Retq => env        
    }

  def goto(c: Code, l: Label): Code =
    c match {
      case Nil => throw(new Error())
      case LabelInstr(l1)::c if (l == l1) => c
      case _::c => goto(c, l)
    }

  def evalCC(env: Env, cc: CC): Boolean =
    cc match {
      case EqCC => (env("%ZF") == 1)
      case LtCC => (env("%SF") == 1)
    }

  type FEnv = Map[Var, Code]

  val realAllRegs = allRegs ++ List("%rsp", "%rbp")  

  def execCode (fenv: FEnv, env: Env, mem: Mem, alloc: Int, c: Code): (Env, Mem, Int) =
    c match {
      case Nil => (env, mem, alloc)
      case Loadq (MemReg(r,i), d)::c => {
        val addr = env(r) + i
        execCode(fenv, env + (d -> mem(addr)), mem, alloc, c)
      }
      case Storeq (s, MemReg(r,i))::c => {
        val addr = env(r) + i
        execCode(fenv, env, mem + (addr -> env(s)), alloc, c)
      }
      case Loadq (MemTmp(x), d)::c => 
        execCode(fenv, env + (d -> env("_"+x)), mem, alloc, c)
      case Storeq (s, MemTmp(x))::c => 
        execCode(fenv, env +  ("_"+x -> env(s)), mem, alloc, c)
      case Loadq (MemBp(i), d)::c => {
        val addr = env("%rbp") + (i+1) * -8
        execCode(fenv, env + (d -> mem(addr)), mem, alloc, c)
      }
      case Storeq (s, MemBp(i))::c => {
        val addr = env("%rbp") + (i+1) * -8
        execCode(fenv, env, mem + (addr -> env(s)), alloc, c)
      }
      case Popq (d)::c => {
        val env1 = env + (d -> mem(env("%rsp")))
        execCode(fenv, env1 + ("%rsp" -> (env1("%rsp") + 8)), mem, alloc, c)
      }
      case Pushq (s)::c => {
        val env1 = env + ("%rsp" -> (env("%rsp") - 8))
        execCode(fenv, env1, mem + (env1("%rsp") -> env1(s)), alloc, c)
      }
      case Callq("cons", 2)::c => {
        val mem1 = mem + (alloc -> env(argRegs(0)))
        val mem2 = mem1 + ((alloc + 8) -> env(argRegs(1)))
        execCode(fenv, env + (retReg -> alloc), mem2, alloc + 16, c)
      }
      case Callq(f, _)::c => {
        val (env1, mem1, alloc1) = execCode(fenv, env, mem, alloc, fenv(f))
        val env2 = env ++ env1.filterKeys(realAllRegs.contains(_))
        execCode(fenv, env2, mem1, alloc1, c)
      }
      case Jmp(l)::c => execCode(fenv, env, mem, alloc, goto(c,l))
      case Jcc(cc, l)::c =>
        if (evalCC(env, cc))
          execCode(fenv, env, mem, alloc, goto(c,l))
        else
          execCode(fenv, env, mem, alloc, c)
      case i::c => execCode(fenv, execInstr(env, i), mem, alloc, c)
    }

  def defs2env (ds: List[Def]): Map[Var, Code] =
    ds.map(d => (d.name, d.body)).toMap

  def mem2list (mem: Mem, a: Int): List[Int] =
    if (a == 0) Nil
    else mem(a)::mem2list(mem,mem(a+8))

}

