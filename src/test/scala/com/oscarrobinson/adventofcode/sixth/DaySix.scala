package com.oscarrobinson.adventofcode.sixth

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DaySix extends AnyWordSpec with Matchers {
  "daySixPartOne" should {
    "return correct number" in {
      daySixPartOne("inputs/6/test_input.txt") shouldEqual 288
    }
  }

  "daySixPartTwo" should {
    "return correct number" in {
      daySixPartTwo("inputs/6/test_input.txt") shouldEqual 71503
    }
  }
}
