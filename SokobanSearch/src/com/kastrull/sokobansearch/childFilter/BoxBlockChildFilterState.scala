package com.kastrull.sokobansearch.childFilter

import scala.collection.Seq
import com.kastrull.sokobansearch.Sokoban
import Sokoban._

final class BoxBlockChildFilterState(
  wrappedState: Sokoban,
  noBoxCoords: Seq[Coord]) extends Sokoban {

  def hasBoxInBadPlace(s: Sokoban) =
    s.boxes.intersect(noBoxCoords).size != 0

  def childStates(): Seq[Sokoban] = {
    val children = wrappedState.childStates
    children.filterNot(hasBoxInBadPlace(_))
  }
  
  
  val sokoban: Sokoban.Coord = wrappedState.sokoban
  val size: Sokoban.Coord = wrappedState.size
  val boxes: List[Sokoban.Coord] = wrappedState.boxes
  val room: Sokoban.Room = wrappedState.room
  val estimatedFutureCost: Int = wrappedState.estimatedFutureCost
}