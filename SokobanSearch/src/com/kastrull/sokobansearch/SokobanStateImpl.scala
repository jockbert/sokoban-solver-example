package com.kastrull.sokobansearch

import Sokoban._
import Coord._

private[sokobansearch] case class SokobanStateImpl(
  sokoban: Coord,
  size: Coord,
  boxes: List[Coord],
  room: Room,
  efcFunc: (Sokoban) => Int)
  extends Sokoban {

  lazy val estimatedFutureCost = efcFunc(this)

  def childStates = {

    def efc = Sokoban.estimatedCost(size, room)(this)

    def moveSokoban(direction: Coord): Option[Sokoban] = {
      val next = sokoban + direction
      val nextElement = room.getOrElse(next, Wall())

      val nextNext = next + direction
      val nextNextElement = room.getOrElse(nextNext, Wall())

      val boxOnNext = boxes.contains(next)
      val boxOnNextNext = boxes.contains(nextNext)

      if (nextElement == Wall()) None
      else if (boxOnNext) {
        if (nextNextElement == Wall() || boxOnNextNext) None
        else {
          val newBoxes = nextNext :: (boxes.filterNot(_ == next))
          Some(SokobanStateImpl(next, size, newBoxes, room, efcFunc))
        }

      } else Some(SokobanStateImpl(next, size, boxes, room, efcFunc))
    }

    for {
      direction <- directions
      child <- moveSokoban(direction)
    } yield child
  }
  override def toString = SokobanPrinter.print(this)
}