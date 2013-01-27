package com.kastrull.sokobansearch

import scala.collection.immutable.SortedSet

class CompleteSokobanStateTest extends SokobanStateTest {

import Sokoban._

  def name() :String = "Complete"
    
  def stateString(): String =
    """	|####
    	|#--#
    	|-Sbx
    	|#-B#""".stripMargin

  def sokoban(): Coord = (1, 2)
  def boxes(): SortedSet[Coord] =
    SortedSet((2, 2), (2, 3))
  def walls(): SortedSet[Coord] =
    SortedSet((0, 0), (0, 1), (0, 3), (1, 0), (2, 0), (3, 0), (3, 1), (3, 3))
  def targets(): SortedSet[Coord] =
    SortedSet((1, 2), (3, 2), (2, 3))
  def size(): Coord =
    (4, 4)
  def estimatedFutureCost(): Int =
    1

  def children(): SortedSet[String] = SortedSet(
    """	|####
    	|#s-#
    	|-xbx
    	|#-B#""".stripMargin,

    """	|####
    	|#--#
    	|sxbx
    	|#-B#""".stripMargin,

    """	|####
    	|#--#
    	|-xsB
    	|#-B#""".stripMargin,

    """	|####
    	|#--#
    	|-xbx
    	|#sB#""".stripMargin)

}