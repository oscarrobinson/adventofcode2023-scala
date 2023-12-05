package com.oscarrobinson.adventofcode

import com.oscarrobinson.adventofcode.fifth.{AlmanacRange, getLowestLocationNumber}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GardenAlmanacTest extends AnyWordSpec with Matchers {

  "AlmanacRange.convert" should {
    "perform correct conversion" in {
      AlmanacRange(50, 52, 48).convert(79) shouldEqual Some(81)
    }
  }

  "AlmanacRange.fromLine" should {
    "parse a range from a line" in {
      AlmanacRange.fromLine("52 50 48 ") shouldEqual AlmanacRange(50, 52, 48)
    }
  }

  "getLowestLocationNumber" should {
    "return correct location" in {
      getLowestLocationNumber("inputs/5/test_input.txt") shouldEqual 35
    }
  }

}
