package com.kastrull.sokobansearch

import com.kastrull.graphsearch.BFSearcher
import com.kastrull.graphsearch.State
import com.kastrull.graphsearch.AStarSearcher
import com.kastrull.sokobansearch.childFilter.NoCornerBoxFilter
import com.kastrull.sokobansearch.childFilter.FilterSokoban
import com.kastrull.sokobansearch.childFilter.NoWallflowerBuddiesFilter

object Program extends App {

  val target: PartialFunction[State[Sokoban], Boolean] = {
    case state: Sokoban => Sokoban.target(state)
  }

  val bfs = new BFSearcher[Sokoban]()
  val aStar = new AStarSearcher[Sokoban]()

  def run(s: Sokoban) {

    val predicate = NoWallflowerBuddiesFilter.isFreeOfWB _

    val sNoCorners = NoCornerBoxFilter(s)
    val sNoBuddies = FilterSokoban(s, predicate)
    val sAllFilters = NoCornerBoxFilter(sNoBuddies)
    val sAllFiltersInversed = FilterSokoban(sNoCorners, predicate)

    val costFunc = Sokoban.estimatedCost(s.size, s.room)

    println()
    println("\t Plain BFS")
    println(bfs.search(s, target))

    println()
    println("\t A*")
    println(aStar.search(s, target, costFunc))

    println()
    println("\t A* With No Corner Box Filter (NCBF)")
    println(aStar.search(sNoCorners, target, costFunc))

    println()
    println("\t A* With No WallflowerBuddies Filter (NWBF)")
    println(aStar.search(sNoBuddies, target, costFunc))

    println()
    println("\t A* With NCBF and NWBF")
    println(aStar.search(sAllFilters, target, costFunc))

    println()
    println("\t A* With NCBF and NWBF")
    println(aStar.search(sAllFiltersInversed, target, costFunc))
  }

  val map1 =
    """	  |----------------------
		  |----------------------
		  |-----xx-----------b-b-
		  |s################-----""".stripMargin
  val map2 =
    """	  |-----
		  |xB-bs
		  |###--""".stripMargin

  val state = Sokoban(map2)

  run(state)
}