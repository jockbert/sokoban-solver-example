package com.kastrull.sokobansearch.childFilter

import org.scalatest.FlatSpec
import com.kastrull.sokobansearch.Sokoban
import com.kastrull.sokobansearch.Sokoban._
import scala.collection.SortedSet
import com.kastrull.sokobansearch.Coord
import com.kastrull.sokobansearch.Coord._

class NoCornerBoxFilterTest extends FlatSpec {

  trait Fixture {
    def map: String
    def expectedBlockedCoords: SortedSet[Coord]

    val room = Sokoban(map).room
    val actualBlockedCoords = SortedSet(NoCornerBoxFilter.noBoxCoords(room): _*)
    assert(expectedBlockedCoords === actualBlockedCoords)
  }

  "No Corner Box Filter" should "identify corners on empty map" in
    new Fixture {

      def map = """	|-bsb--
    				|------""".stripMargin

      def expectedBlockedCoords = SortedSet((0, 0), (0, 1), (5, 0), (5, 1))
    }

  it should "identify all types of corners" in
    new Fixture {

      def map = """	|-s---
    				|--#--
    		  		|-###-
    		  		|--#--
    		  		|-----""".stripMargin

      def expectedBlockedCoords = SortedSet(
        (0, 0), (0, 4), (4, 0), (4, 4),
        (1, 1), (3, 3), (1, 3), (3, 1))
    }

  it should "not identify corners with targets" in
    new Fixture {

      def map = """	|xs--x
    				|-x#x-
    		  		|-###-
    		  		|-x#x-
    		  		|x---x""".stripMargin

      def expectedBlockedCoords = SortedSet()
    }

}