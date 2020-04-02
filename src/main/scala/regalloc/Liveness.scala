package regalloc

object Liveness {
  import codegen.CodegenBase._
  import codegen.Asm._

  // 命令列 c の入口生存変数を計算 
  def liveness (c: Code) : Set[Reg] = {
    // m: ラベルから live-in へのマップ
    var m = Map[Label, Set[Reg]]()
    def live(c: Code): Set[Reg] = 
      c match {
        case Nil => Set(retReg)
        case i::c => {
          val s = live(c)
          i match {
            case LabelInstr(l) => {
              m = m + (l->s)
              s
            }
            case Jmp(l) => m(l)
            case Jcc(cc,l) => s++ m(l)
            case _ => (s -- i.defR) ++ i.use
          }
        }
      }
    live(c)
  }

  def livenessList (c: Code): List[Set[Reg]] = {
    // m: ラベルから live-in へのマップ
    var m = Map[Label, Set[Reg]]()
    def live(c: Code): List[Set[Reg]] = 
      c match {
        case Nil => List(Set(retReg))
        case i::c => {
          val ss = live(c)
          val s = ss.head
          val cur = i match {
            case LabelInstr(l) => {
              m = m + (l->s)
              s
            }
            case Jmp(l) => m(l)
            case Jcc(cc,l) => s++ m(l)
            case _ => (s -- i.defR) ++ i.use
          }
          cur::ss
        }
      }
    live(c)
  }

  // 干渉グラフを構築
  def interGraph (c: Code) : Graph[Reg] = {
    var m = Map[Label, Set[Reg]]()
    var g = new Graph[Reg](Set(), Set())
    def live(c:Code): Set[Reg] = 
      c match {
        case Nil => {
          g = g.addNode(retReg)
          Set(retReg)
        }
        case i::c => {
          val s = live(c)
          i match { 
            case LabelInstr(l) => {
              m = m + (l->s)
              s
            }
            case Jmp(l) => m(l)
            case Jcc(cc,l) => live(c) ++ m(l)
            // move命令の特殊な扱い
            case Movq(RegOpd(rs), rd) => {
              g = g.addNode(rs)
              g = g.addNode(rd)
              for (ru <- s)
                if (ru != rs && ru != rd) g = g.addEdge(rd,ru)
              (s - rd) + rs
            }
            case _ => {
              for (r <- i.defR ++ i.use)
                g = g.addNode(r)
              for (rd <- i.defR)
                for (ru <- s)
                  if (rd != ru) g = g.addEdge(rd,ru)
                    (s -- i.defR) ++ i.use
            }
          }
        }
      }
    val s = live(c)
    // 生きている変数は，関数呼びだしの引数に使われるレジスタのみのはず
    assert(s subsetOf argRegs.toSet)
    g
  }

  def moves (c: Code): List[(Reg, Reg)] =
    c match {
      case Nil => Nil
      case Movq(RegOpd(r1), r2)::c => {
        (r1,r2)::moves(c)
      }
      case _::c => moves(c) 
    }

}
