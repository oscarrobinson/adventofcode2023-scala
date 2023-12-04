package com.oscarrobinson.adventofcode.fourth

import com.oscarrobinson.adventofcode.utils.Utils

import scala.io.Source

case class Scratchcard(winningNumbers: Set[Int], numbersPresent: Set[Int]) {
  def score: Int = {
    val matchCount = winningNumbers.intersect(numbersPresent).size
    Math.floor(Math.pow(2, matchCount - 1)).toInt
  }
}

object Scratchcard {
  def fromLine(line: String): Scratchcard = {
    val splitLine = line.split('|')
    val winningNumbers = splitLine(0).split(':').last.split("""\s+""").filter(_.nonEmpty).map(_.toInt).toSet
    val numbersPresent = splitLine(1).split("""\s+""").filter(_.nonEmpty).map(_.toInt).toSet
    Scratchcard(winningNumbers, numbersPresent)
  }
}


def getScratchcardsTotalPoints(filename: String): Int = {
  val lines = Utils.fileLinesAs[List[String]](filename, _.toList)
  lines.map(Scratchcard.fromLine).map(_.score).sum
}


@main
def main(): Unit = {
  println(s"Part 1: ${getScratchcardsTotalPoints("inputs/4/input.txt")}")
}

