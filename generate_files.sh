#!/bin/bash


number=$1 #e.g Nine
day=$2    #e.g ninth
date=$3   #e.g 9

mkdir src/main/scala/com/oscarrobinson/adventofcode/$2
touch src/main/scala/com/oscarrobinson/adventofcode/$2/Day$1.scala

mkdir src/test/scala/com/oscarrobinson/adventofcode/$2
touch src/test/scala/com/oscarrobinson/adventofcode/$2/Day$1Test.scala

mkdir inputs/$3
touch inputs/$3/input.txt
touch inputs/$3/input_test.txt

cat >src/main/scala/com/oscarrobinson/adventofcode/$2/Day$1.scala <<EOL
package com.oscarrobinson.adventofcode.${day}

import com.oscarrobinson.adventofcode.utils.Utils

def day${number}PartOne(filename: String): Long = {
  0L
}

def day${number}PartTwo(filename: String): Long = {
  0L
}

@main
def main(): Unit = {
  println(s"Part 1: \${day${number}PartOne("inputs/${date}/input.txt")}")
  println(s"Part 2: \${day${number}PartTwo("inputs/${date}/input.txt")}")
}
EOL

cat >src/test/scala/com/oscarrobinson/adventofcode/$2/Day$1Test.scala <<EOL
package com.oscarrobinson.adventofcode.${day}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day${number}Test extends AnyWordSpec with Matchers {
  "day${number}PartOne" should {
    "return correct number" in {
      day${number}PartOne("inputs/${date}/test_input.txt") shouldEqual 1L
    }
  }

  "day${number}PartTwo" should {
    "return correct number" in {
      day${number}PartTwo("inputs/${date}/test_input.txt") shouldEqual 1L
    }
  }
}
EOL