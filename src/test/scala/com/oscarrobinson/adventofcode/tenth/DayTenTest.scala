package com.oscarrobinson.adventofcode.tenth

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DayTenTest extends AnyWordSpec with Matchers {
  "dayTenPartOne" should {
    "return correct number" in {
      dayTenPartOne("inputs/10/test_input.txt") shouldEqual 8L
    }
  }

  "dayTenPartTwo" should {
    "return correct number" in {
      dayTenPartTwo("inputs/10/test_input_2.txt") shouldEqual 10L
    }

    "return correct number when some enclosed area shouldnt be counted" in {
      dayTenPartTwo("inputs/10/test_input_3.txt") shouldEqual 4L
    }
  }
}
