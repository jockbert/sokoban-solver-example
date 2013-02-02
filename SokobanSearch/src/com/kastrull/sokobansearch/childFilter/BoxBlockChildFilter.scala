package com.kastrull.sokobansearch.childFilter

import scala.collection.Seq
import com.kastrull.sokobansearch.Sokoban
import com.kastrull.sokobansearch.Coord

case class BoxBlockChildFilter(
  wrapped: Sokoban,
  noBoxCoords: Seq[Coord]) extends Sokoban {

  def hasBoxInBadPlace(s: Sokoban) =
    s.boxes.intersect(noBoxCoords).size != 0

  def childStates(): Seq[Sokoban] = {
    val children = wrapped.childStates
    //    val blocked = children.filter(hasBoxInBadPlace(_)).map(BoxBlockChildFilter(_,noBoxCoords))
    //    if(blocked.size > 0) println(blocked.mkString("\n","\n","\n"))
    children.filterNot(hasBoxInBadPlace(_)).map(BoxBlockChildFilter(_, noBoxCoords))
  }

  val sokoban: Coord = wrapped.sokoban
  val size: Coord = wrapped.size
  val boxes: List[Coord] = wrapped.boxes
  val room: Sokoban.Room = wrapped.room
  val estimatedFutureCost: Int = wrapped.estimatedFutureCost
  override def toString = wrapped.toString
}