package regalloc

// 無向グラフ
class Graph[A](n: Set[A], e: Set[(A,A)]) {
  val nodes: Set[A] = n
  val edges: Set[(A,A)] = e

  // グラフに節点を追加
  def addNode(n: A): Graph[A] = new Graph(nodes + n, e)

  // グラフに辺を追加
  def addEdge(e: (A,A)): Graph[A] = {
    val (n1, n2) = e
    val e1: (A, A) = (n2, n1) 
    new Graph(nodes, edges + e + e1)
  }

  // 節点nに隣接する節点の集合
  def adjacent(n: A) : Set[A] = edges.filter(_._1 == n).map(_._2)

  // 節点nの次数
  def degree(n: A) : Int = adjacent(n).size

  // 節点nを削除
  def removeNode(n: A) : Graph[A] =
    new Graph(nodes - n, edges.filter(e => e._1 != n && e._2 != n))

  // 節点n1とn2を合弁：n1を残す
  def merge(n1: A, n2: A): Graph[A] = {
    def rename(n: A) = if (n == n2) n1 else n
    new Graph(nodes - n2,
      edges.map{ case (m1, m2) => (rename(m1), rename(m2)) })
  }

    
}


