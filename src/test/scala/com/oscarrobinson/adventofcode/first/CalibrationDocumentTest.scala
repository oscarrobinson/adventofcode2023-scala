package com.oscarrobinson.adventofcode.first

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CalibrationDocumentTest extends AnyWordSpec with Matchers {

  "calibrationSumFromFile" should {
    "return correct sum" in {
      calibrationSumFromFile("inputs/1/test_input.txt") shouldEqual 344
    }
  }


}
