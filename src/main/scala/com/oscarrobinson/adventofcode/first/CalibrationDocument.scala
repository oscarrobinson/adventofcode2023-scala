package com.oscarrobinson.adventofcode.first

import scala.io.Source


def lineToNumber(line: String): Int = {
  val numbers = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    .zipWithIndex.map { (numString, index) => (numString, index % 10) }.toMap
  val foundNumbers = (0 to line.length).flatMap(index =>
    numbers.find { (word, _) =>
      line
        .slice(index, line.length)
        .startsWith(word)
    }.map(_._2))
  foundNumbers.head*10+foundNumbers.last
}

def calibrationSumFromFile(filePath: String): Int = {
  val file = Source.fromFile(filePath)
  file.getLines().map(lineToNumber).sum
}

@main
def main(): Unit = {
  val sum = calibrationSumFromFile("inputs/1/input.txt")
  println(sum)
}