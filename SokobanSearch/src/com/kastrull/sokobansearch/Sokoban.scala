package com.kastrull.sokobansearch

import scala.collection.Seq
import com.kastrull.graphsearch.State
import Coord._

trait Sokoban extends State[Sokoban] {
  val sokoban: Coord
  val size: Coord
  val boxes: List[Coord]
  val room: Sokoban.Room
  val estimatedFutureCost: Int
}

object Sokoban {

  def apply(mapString: String): Sokoban = {
    val charCoordSeq = for {
      (row, rowIndex) <- rowsWithIndex(mapString)
      (char, coord) <- charWithCoord(row, rowIndex)
    } yield (char, coord)

    val emptySokobanState =
      new SokobanStateImpl(origo, origo, Nil, Map(), { s => 0 })

    charCoordSeq.foldLeft(emptySokobanState)(improveState)
  }

  def target(state: Sokoban): Boolean =
    state.boxes.forall { state.room(_) == Target() }

  def rowsWithIndex(roomDescr: String): Seq[(String, Int)] = {
    val rowsArray = roomDescr.split("\n")
    val rows: List[String] = rowsArray.toList
    val rowIndexes = 0 until rows.size
    rows zip rowIndexes
  }

  def charWithCoord(row: String, rowIndex: Int): Seq[(Char, Coord)] = {
    val indexes = 0 until row.size
    val coords = for (index <- indexes) yield Coord(index, rowIndex)
    row zip coords
  }

  def improveState(
    state: SokobanStateImpl,
    charAndCoord: (Char, Coord)) = {

    val (char, charCoord) = charAndCoord
    val Coord(charX, charY) = charCoord
    val SokobanStateImpl(sokoban, Coord(sizeX, sizeY), boxes, map, _) = state

    val newSokoban = char match {
      case 'S' => charCoord
      case 's' => charCoord
      case _ => sokoban
    }

    import math.max
    val newSize: Coord = Coord(max(sizeX - 1, charX) + 1, max(sizeY - 1, charY) + 1)

    val newBoxes = char match {
      case 'B' => charCoord :: boxes
      case 'b' => charCoord :: boxes
      case _ => boxes
    }

    val element: Element = char match {
      case '-' => Empty()
      case 's' => Empty()
      case 'b' => Empty()
      case '#' => Wall()
      case _ => Target()
    }

    val newMap = map + (charCoord -> element)
    val newEcfFunc = estimatedCost(newSize, newMap)

    new SokobanStateImpl(
      newSokoban, newSize,
      newBoxes, newMap, newEcfFunc)
  }

  def estimatedCost(size: Coord, room: Room): (Sokoban) => Int = {

    def manhattan(a: Coord, b: Coord): Int =
      math.abs(a.x - b.x) + math.abs(a.y - b.y)

    val coords: List[Coord] =
      (for {
        y <- 0 to (size.y)
        x <- 0 to (size.x)
      } yield Coord(x, y)).toList

    val targets = coords.filter { room.getOrElse(_, Empty()) == Target() }

    val distanceToTargets =
      for {
        coord <- coords
        target <- targets
      } yield (coord, manhattan(coord, target))

    val distanceToTargetsMap = distanceToTargets.groupBy(_._1)
    val shortestDistance = distanceToTargetsMap.map { m =>
      val (coord, tupleList) = m
      val distances = tupleList.map { _._2 }
      (coord, distances.min)
    }

    def precalcCost(state: Sokoban) = {
      state match {
        case s: Sokoban => {
          val boxDistance = s.boxes.map(shortestDistance(_)).sum
          val sokobanDistance =
            if (s.boxes.size > 0) (s.boxes.map(manhattan(s.sokoban, _)).min - 1)
            else 0
          boxDistance + sokobanDistance
        }
        case _ => 0
      }
    }

    precalcCost
  }

  type Room = Map[Coord, Element]
}
