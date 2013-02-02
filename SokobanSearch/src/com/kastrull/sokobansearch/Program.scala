package com.kastrull.sokobansearch

import com.kastrull.graphsearch.BFSearcher
import com.kastrull.graphsearch.State
import com.kastrull.graphsearch.AStarSearcher
import com.kastrull.sokobansearch.childFilter.NoCornerBoxFilter

object Program extends App {

  val target: PartialFunction[State[Sokoban], Boolean] = {
    case state: Sokoban => Sokoban.target(state)
  }

  val bfs = new BFSearcher[Sokoban]()
  val aStar = new AStarSearcher[Sokoban]()

  def run(s: Sokoban) {

    val ss = NoCornerBoxFilter(s)
    
    val costFunc = Sokoban.estimatedCost(s.size, s.room)

    println(bfs.search(s, target))
    println(aStar.search(s, target, costFunc))
    println(aStar.search(ss, target, costFunc))
  }

  // Statistics
  //
  // iter: 224532	Plain BFS
  // iter: 78235 	A* Without sokoban-to-box-distance (S2BD)
  // iter: 50291 	A* With S2BD
  // iter: 44279	A* With S2BD and No Corner Box Filter (NCBF)

  val text =
    """	  |----------------------
		  |----------------------
		  |-----xx-----------b-b-
		  |s################-----""".stripMargin

  val state = Sokoban(text)

  run(state)
}