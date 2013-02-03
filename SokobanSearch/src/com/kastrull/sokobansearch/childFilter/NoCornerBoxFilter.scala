package com.kastrull.sokobansearch.childFilter

import com.kastrull.sokobansearch.Coord
import com.kastrull.sokobansearch.Sokoban
import com.kastrull.sokobansearch.Sokoban._
import com.kastrull.sokobansearch.Empty
import com.kastrull.sokobansearch.Wall

object NoCornerBoxFilter {
  import Coord._

  private val corners = List(
    (north, west), (north, east),
    (south, west), (south, east))

  private def isInCorner(coord: Coord, room: Room): Boolean = {
    def isEmpty(c: Coord) = room.getOrElse(coord, Wall()) == Empty()
    def isWall(c: Coord) = room.getOrElse(c, Wall()) == Wall()
    def isCorner(c: Coord) = corners.exists { corner =>
      val n1 = corner._1 + c
      val n2 = corner._2 + c
      isWall(n1) && isWall(n2)
    }
    isEmpty(coord) && isCorner(coord)
  }

  def noBoxCoords(room: Room): Seq[Coord] =
    room.keySet.toList.filter { isInCorner(_, room) }

  def apply(sokoban: Sokoban): Sokoban =
    BoxBlockChildFilter(sokoban, noBoxCoords(sokoban.room))
}