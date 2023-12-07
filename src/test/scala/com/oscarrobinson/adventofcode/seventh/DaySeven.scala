package com.oscarrobinson.adventofcode.seventh

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DaySeven extends AnyWordSpec with Matchers {

  "Hand.typeRank" should {

    "return correct rank for high card" in {
      Hand("23456", 100).typeRank() shouldEqual 0
    }

    "return correct rank for one pair" in {
      Hand("32T3K", 100).typeRank() shouldEqual 1
    }

    "return correct rank for two pair" in {
      Hand("KK677", 100).typeRank() shouldEqual 2
    }

    "return correct rank for three of a kind" in {
      Hand("T55J5", 100).typeRank() shouldEqual 3
    }

    "return correct rank for full house" in {
      Hand("23332", 100).typeRank() shouldEqual 4
    }

    "return correct rank for four of a kind" in {
      Hand("AA8AA", 100).typeRank() shouldEqual 5
    }

    "return correct rank for five of a kind" in {
      Hand("AAAAA", 100).typeRank() shouldEqual 6
    }
  }

  "daySevenPartOne" should {
    "return correct number" in {
      daySevenPartOne("inputs/7/test_input.txt") shouldEqual 6440L
    }
  }

  "daySevenPartTwo" should {
    "return correct number" in {
      daySevenPartTwo("inputs/7/test_input.txt") shouldEqual 1L
    }
  }
}
