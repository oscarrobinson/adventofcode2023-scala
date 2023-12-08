package com.oscarrobinson.adventofcode.eighth

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DayEightTest extends AnyWordSpec with Matchers {

  "dayEightPartOne" should {
    "return correct result" in {
      dayEightPartOne("inputs/8/test_input.txt") shouldEqual 6L
    }
  }

  "dayEightPartTwo" should {
    "return correct result" in {
      dayEightPartTwo("inputs/8/test_input_2.txt") shouldEqual 6L
    }
  }
}
