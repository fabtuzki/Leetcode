import scala.collection.mutable

class DirectedDFS(G: DiGraph, vertices: Array[Int]) {
  //  def this(G: DiGraph, vertex: Int) = this(G, Array(vertex))

  private var marked = Array.ofDim[Boolean](vertices.length)

  def dfs(G: DiGraph, v: Int): Unit = {
    marked(v) = true
    for (w <- G.adj(v)) {
      if (!marked(w)) {
        dfs(G, w)
      }
    }
  }

  def DirectedDFS(G: DiGraph, vertices: Array[Int]): Unit = {
    for (s <- vertices) {
      if (!marked(s)) {
        dfs(G, s)
      }
    }
  }

  def markedCheck(v: Int): Boolean = {
    marked(v)
  }


}



