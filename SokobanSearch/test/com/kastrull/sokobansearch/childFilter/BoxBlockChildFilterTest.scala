package com.kastrull.sokobansearch.childFilter

import org.scalatest.FlatSpec
import com.kastrull.sokobansearch.Sokoban
import com.kastrull.sokobansearch.Coord
import scala.collection.SortedSet

class BoxBlockChildFilterTtest extends FlatSpec {

  "BoxBlockChildFilter" should "filter out children with blocked boxes" in {
    val map = "-bSb--"
    val sokoban = Sokoban(map)

    val blocked = List[Coord]((0, 0))

    val filteredSokoban = BoxBlockChildFilter(sokoban, blocked)

    val actualChildren = SortedSet(filteredSokoban.childStates().map(_.toString): _*)
    val expectedChildren = SortedSet("-bxsb-")
    
    val expectedUnfilteredChildren = SortedSet("-bxsb-","bsxb--")
    val actualUnfilteredChildren = SortedSet(sokoban.childStates().map(_.toString): _*)
    
    assert(actualUnfilteredChildren === expectedUnfilteredChildren)
    assert(actualChildren === expectedChildren)
  }

}