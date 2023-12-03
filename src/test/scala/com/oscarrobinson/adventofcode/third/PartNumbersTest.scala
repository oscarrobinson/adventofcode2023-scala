package com.oscarrobinson.adventofcode.third

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PartNumbersTest extends AnyWordSpec with Matchers {

  "sumPartNumbers" should {
    "return correct sum" in {
      sumPartNumbers(Schematic.fromFileName("inputs/3/test_input.txt")) shouldEqual 4361
    }
  }

  "sumGearRatios" should {
    "return correct sum" in {
      sumGearRatios(Schematic.fromFileName("inputs/3/test_input.txt")) shouldEqual 467835
    }
  }
}
