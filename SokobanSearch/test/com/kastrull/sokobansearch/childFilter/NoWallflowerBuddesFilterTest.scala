package com.kastrull.sokobansearch.childFilter

import org.scalatest.FlatSpec

import com.kastrull.sokobansearch.Sokoban

class NoWallflowerBuddesFilterTest extends FlatSpec {

  trait Fixture {
    def map: String
    def expected: Boolean

    val sokoban = Sokoban(map)
    val actual: Boolean = NoWallflowerBuddiesFilter.hasWallflowerBuddies(sokoban)
    assert(expected === actual)
  }

  "No Wallflowe Buddies Filter" should "not give false positives" in
    new Fixture {
      def expected = false
      def map = """	|----
    				|-bb-
    		  		|---S""".stripMargin
    }

  it should "not give true positives - north wall" in
    new Fixture {
      def expected = true
      def map = """	|-##-
    				|-bb-
    		  		|---S""".stripMargin
    }

  it should "not give true positives - south wall" in
    new Fixture {
      def expected = true
      def map = """	|----
    				|-bb-
    		  		|-##S""".stripMargin
    }

  it should "not give true positives - west wall" in
    new Fixture {
      def expected = true
      def map = """	|----
    				|#b--
    		  		|#b--
        			|---S""".stripMargin
    }

  it should "not give true positives - east wall" in
    new Fixture {
      def expected = true
      def map = """	|----
    				|-b#-
    		  		|-b#-
        			|---S""".stripMargin
    }

  it should "true even when one box is on target - 1" in
    new Fixture {
      def expected = true
      def map = """	|-##-
    				|-bB-
    		  		|---S""".stripMargin
    }

  it should "true even when one box is on target - 2" in
    new Fixture {
      def expected = true
      def map = """	|-##-
    				|-Bb-
    		  		|---S""".stripMargin
    }

  it should "false when both boxes are on target" in
    new Fixture {
      def expected = false
      def map = """	|-##-
    				|-BB-
    		  		|---S""".stripMargin
    }
}