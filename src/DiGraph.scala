class DiGraph(V: Int) {
  var E = 0
  var adj = Array.ofDim[List[Int]](V)


  def addEdge(v: Int, w: Int): Unit = {
    adj(v).::(w)
    E += 1
  }

  def adjList(v: Int): List[Int] = {
    adj(v)
  }

  def reverse(): DiGraph = {
    val R = new DiGraph(V)
    for (i <- 0 until V) {
      for (w <- adj(i)) {
        R.addEdge(w, i)

      }
    }
    R
  }

}


