package com.kastrull.sokobansearch

import scala.collection.immutable.SortedSet

class TinySokobanStateTest extends SokobanStateTest {
import Sokoban._
  val emptyCoords = SortedSet[Coord]()
   
  def name() :String = "Tiny"
  def stateString(): String = "s"
  def sokoban(): Coord = (0,0)
  def boxes(): SortedSet[Coord] = emptyCoords
  def walls(): SortedSet[Coord] = emptyCoords
  def targets(): SortedSet[Coord] = emptyCoords
  def size(): Coord = (1,1)
  def estimatedFutureCost(): Int = 0
  def children(): SortedSet[String] = SortedSet()
  
}