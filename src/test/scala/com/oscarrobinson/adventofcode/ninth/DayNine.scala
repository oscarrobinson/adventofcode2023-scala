package com.oscarrobinson.adventofcode.ninth

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DayNine extends AnyWordSpec with Matchers {
  "dayNinePartOne" should {
    "return correct number" in {
      dayNinePartOne("inputs/9/test_input.txt") shouldEqual 114L
    }
  }

  "dayNinePartTwo" should {
    "return correct number" in {
      dayNinePartTwo("inputs/9/test_input.txt") shouldEqual 2L
    }
  }
}
