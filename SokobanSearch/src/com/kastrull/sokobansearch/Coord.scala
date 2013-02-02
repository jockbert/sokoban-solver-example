package com.kastrull.sokobansearch

object Coord {
  val north: Coord = Coord(-1, 0)
  val south: Coord = Coord(1, 0)
  val west: Coord = Coord(0, -1)
  val east: Coord = Coord(0, 1)
  val origo: Coord = Coord(0, 0)
  val directions = List(north, south, west, east)

  implicit def coord(pair: (Int, Int)) = Coord(pair._1, pair._2)
  implicit def ordering: Ordering[Coord] = Ordering.fromLessThan { (a, b) => a < b }
}

case class Coord(x: Int, y: Int) {
  def +(o: Coord) = Coord(x + o.x, y + o.y)
  def <(o: Coord) = x < o.x || (x == o.x && y < o.y)
}