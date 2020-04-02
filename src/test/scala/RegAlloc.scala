import org.scalatest._

class RegAllocTest extends FlatSpec with RegAllocBehavior {
  def codegenFileDefs(s: String) = Main.codegenFileDefs(s)
}
