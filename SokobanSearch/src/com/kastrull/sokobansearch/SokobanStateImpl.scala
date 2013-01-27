package com.kastrull.sokobansearch

import Sokoban._

private[sokobansearch] case class SokobanStateImpl(
  sokoban: Coord,
  size: Coord,
  boxes: List[Coord],
  room: Room,
  efcFunc: (Sokoban) => Int)
  extends Sokoban {
  
  lazy val estimatedFutureCost = efcFunc(this)

  def childStates = {
    def add(c1: Coord, c2: Coord) = (c1._1 + c2._1, c1._2 + c2._2)
    
    def efc = Sokoban.estimatedCost(size, room)(this)

    def moveSokoban(direction: Coord): Option[Sokoban] = {
      val next = add(sokoban, direction)
      val nextElement = room.getOrElse(next, Wall())

      val nextNext = add(next, direction)
      val nextNextElement = room.getOrElse(nextNext, Wall())

      val boxOnNext = boxes.contains(next)
      val boxOnNextNext = boxes.contains(nextNext)

      if (nextElement == Wall()) None
      else if (boxOnNext) {
        if (nextNextElement == Wall() || boxOnNextNext) None
        else {
          val newBoxes = nextNext :: (boxes.filterNot(_ == next))
          Some(SokobanStateImpl(next, size, newBoxes, room,efcFunc))
        }

      } else Some(SokobanStateImpl(next, size, boxes, room,efcFunc))
    }

    for {
      direction <- directions
      child <- moveSokoban(direction)
    } yield child
  }
  override def toString = SokobanPrinter.print(this)
}