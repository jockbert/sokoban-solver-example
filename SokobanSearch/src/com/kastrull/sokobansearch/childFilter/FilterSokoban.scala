package com.kastrull.sokobansearch.childFilter

import com.kastrull.sokobansearch.Coord
import com.kastrull.sokobansearch.Sokoban

import FilterSokoban.Predicate

object FilterSokoban {
  type Predicate = (Sokoban) => Boolean
}

case class FilterSokoban(
  wrapped: Sokoban,
  predicate: Predicate) extends Sokoban {

  def childStates = {
    //val blocked = wrapped childStates () filterNot predicate 
    //if (blocked.size > 0) println(blocked.mkString("\n", "\n", "\n"))

    val filtered = wrapped childStates () filter predicate
    filtered map { FilterSokoban(_, predicate) }
  }

  val sokoban: Coord = wrapped.sokoban
  val size: Coord = wrapped.size
  val boxes: List[Coord] = wrapped.boxes
  val room: Sokoban.Room = wrapped.room
  val estimatedFutureCost: Int = wrapped.estimatedFutureCost
  override def toString = wrapped.toString + " with PredicateFilter"
}