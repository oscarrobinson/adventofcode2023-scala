package com.oscarrobinson.adventofcode.second

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CubeConundrumTest extends AnyWordSpec with Matchers {

  "sumPossibleGameIds" should {
    "return correct sum" in {
      sumPossibleGameIds("inputs/2/test_input.txt", CubeCounts(12, 13, 14)) shouldEqual 8
    }
  }

  "sumPowerSetsOfMinPossibleGameConfigs" should {
    "return correct sum" in {
      sumPowerSetsOfMinPossibleGameConfigs("inputs/2/test_input.txt") shouldEqual 2286
    }
  }


}
