package com.kastrull.sokobansearch

object SokobanPrinter {
  import Sokoban._

  def print(state: Sokoban): String = {
    val rows =
      for (y <- (0 until state.size.y)) yield rowString(state, y)

    rows.mkString("\n")
  }

  private def coordChar(state: Sokoban, coord: Coord) = {

    val hasSokoban = state.sokoban == coord
    val hasBox = state.boxes.contains(coord)
    val element = state.room(coord)

    element match {
      case Empty() =>
        if (hasSokoban) 's'
        else if (hasBox) 'b'
        else '-'
      case Target() =>
        if (hasSokoban) 'S'
        else if (hasBox) 'B'
        else 'x'
      case Wall() => '#'
    }
  }

  private def rowString(state: Sokoban, y: Int): String = {

    val rowCoords = for { x <- 0 until state.size.x } yield Coord(x, y)

    val rowChars = for { coord <- rowCoords } yield coordChar(state, coord)

    rowChars mkString
  }
}