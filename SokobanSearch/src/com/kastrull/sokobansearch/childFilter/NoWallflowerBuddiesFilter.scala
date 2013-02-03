package com.kastrull.sokobansearch.childFilter

import com.kastrull.sokobansearch.Sokoban
import com.kastrull.sokobansearch.Coord
import com.kastrull.sokobansearch.Coord._
import com.kastrull.sokobansearch.Wall
import com.kastrull.sokobansearch.Target

object NoWallflowerBuddiesFilter {
  
  def isFreeOfWB(sokoban: Sokoban): Boolean = 
    ! hasWallflowerBuddies(sokoban)

  def hasWallflowerBuddies(sokoban: Sokoban): Boolean = {

    case class Buddy(buddy: Coord, wall: Coord)
    val buddies = List(
      Buddy(north, east), Buddy(west, north),
      Buddy(south, west), Buddy(east, south))

    val boxes = sokoban.boxes

    def getElement(c: Coord) = sokoban.room.getOrElse(c, Wall())
    def isTarget(c: Coord) = getElement(c) == Target()
    def isWall(c: Coord) = getElement(c) == Wall()

    def hasSpecificWallflowerBuddy(box: Coord, rule: Buddy) = {
      val buddy = box + rule.buddy
      val hasBuddyBox = boxes.contains(buddy)
      val wall1 = box + rule.buddy + rule.wall
      val wall2 = box + rule.wall
      val hasWall1 = isWall(wall1)
      val hasWall2 = isWall(wall2)
      val bothBoxesIsOnTarget = isTarget(box) && isTarget(buddy)

      hasBuddyBox && hasWall1 && hasWall2 && ! bothBoxesIsOnTarget
    }

    def hasAnyWallflowerBuddy(box: Coord) =
      buddies exists { rule => hasSpecificWallflowerBuddy(box, rule) }

    boxes exists hasAnyWallflowerBuddy
  }
}

class NoWallflowerBuddiesFilter() {

}