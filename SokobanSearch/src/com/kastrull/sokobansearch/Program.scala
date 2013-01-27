package com.kastrull.sokobansearch

import com.kastrull.graphsearch.BFSearcher
import com.kastrull.graphsearch.State
import com.kastrull.graphsearch.AStarSearcher

object Program extends App {

  val target: PartialFunction[State[Sokoban], Boolean] = {
    case state: Sokoban => Sokoban.target(state)
  }

  val bfs = new BFSearcher[Sokoban]()
  val aStar = new AStarSearcher[Sokoban]()

  def run(s: Sokoban) {
    println(bfs.search(s, target))

    val costFunc = Sokoban.estimatedCost(s.size, s.room)
    println(aStar.search(s, target, costFunc))
  }

  
  // Statistik 
  //
  // iter: 224532	Plain BFS
  // iter: 78235 	A* Without sokoban-to-box-distance (S2BD)
  // iter: 50291 	A* With S2BD
  // iter:			A* With S2BD and corner filter

  val text =
    """	  |----------------------
		  |----------------------
		  |-----xx-----------b-b-
		  |s################-----""".stripMargin

  val state = Sokoban(text)
  
  run(state)
}