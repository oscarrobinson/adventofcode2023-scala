package com.oscarrobinson.adventofcode.fourth

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScratchcardsTest extends AnyWordSpec with Matchers {
  "getScratchcardsTotalPoints" should {
    "return correct total" in {
      getScratchcardsTotalPoints("inputs/4/test_input.txt") shouldEqual 13
    }
  }

}
