package com.kastrull.sokobansearch

import org.scalatest.FlatSpec
//import com.kastrull.graphsearch.State
import scala.collection.immutable.SortedSet

abstract class SokobanStateTest extends FlatSpec {
  import Sokoban._

  def name() : String
  def state() = Sokoban(stateString())
  def stateString(): String
  def sokoban(): Coord
  def size(): Coord
  def children(): SortedSet[String]
  def estimatedFutureCost(): Int
  def walls(): SortedSet[Coord]
  def boxes(): SortedSet[Coord]
  def targets(): SortedSet[Coord]

  trait Fixture[S] {
    def expected: S
    def actual: S
    assert(expected === actual)
  }

  (name() + " SokobanState") should
    ("have sokoban at " + sokoban()) in
    new Fixture[Coord] {
      def expected = sokoban()
      def actual = state.sokoban
    }

  it should ("have a specific set of " + children().size + " children") in
    new Fixture[SortedSet[String]] {
      def expected = children()
      def actual = SortedSet[String]() ++
        state().childStates().map { _.toString() }
    }

  it should "have specific toString" in
    new Fixture[String] {
      def expected = stateString()
      def actual = state.toString
    }

  it should ("have specific size " + size) in
    new Fixture[Coord] {
      def expected = size
      def actual = state().size
    }

  it should ("have a specific estimatedFutureCost " + estimatedFutureCost) in
    new Fixture[Int] {
      def expected = estimatedFutureCost()
      def actual = state().estimatedFutureCost
    }

  it should "have specific walls" in
    new Fixture[SortedSet[Coord]] {
      def expected = walls()
      def actual =
        SortedSet[Coord]() ++
          state().room.
          filter { _._2 == Wall() }.
          toList.map { _._1 }
    }

  it should "have specific targets" in
    new Fixture[SortedSet[Coord]] {
      def expected = targets()
      def actual =
        SortedSet[Coord]() ++
          state().room.
          filter { _._2 == Target() }.
          toList.map { _._1 }
    }

  it should "have specific boxes" in
    new Fixture[SortedSet[Coord]] {
      def expected = boxes()
      def actual = SortedSet[Coord]() ++
        state().boxes
    }
}