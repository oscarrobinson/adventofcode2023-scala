package com.oscarrobinson.adventofcode.fourth

import com.oscarrobinson.adventofcode.utils.Utils

import scala.annotation.tailrec
import scala.io.Source

case class Scratchcard(cardNumber: Int, winningNumbers: Set[Int], numbersPresent: Set[Int], count: Int = 1) {
  def matches = winningNumbers.intersect(numbersPresent).size

  def score: Int =
    Math.floor(Math.pow(2, matches - 1)).toInt

  override def toString: String = s"Scratchcard(number=$cardNumber matches=$matches count=$count)"
}

object Scratchcard {

  val cardNumberPattern = """Card\s*([0-9]+)""".r

  def fromLine(line: String): Scratchcard = {
    val cardNumber = line.split(':')(0).slice(4, 100).strip().toInt
    val splitLine = line.split('|')
    val winningNumbers = splitLine(0).split(':').last.split("""\s+""").filter(_.nonEmpty).map(_.toInt).toSet
    val numbersPresent = splitLine(1).split("""\s+""").filter(_.nonEmpty).map(_.toInt).toSet
    Scratchcard(cardNumber.toInt, winningNumbers, numbersPresent)
  }
}


def getScratchcardsTotalPoints(filename: String): Int = {
  val lines = Utils.fileLinesAs[List[String]](filename, _.toList)
  lines.map(Scratchcard.fromLine).map(_.score).sum
}


def calculateScratchcardCount(scratchcards: List[Scratchcard]): Int = scratchcards match
  case Nil => 0
  case thisCard :: remainingCards =>
    val matches = thisCard.matches
    val cardsWinningNewCopy =
      remainingCards.slice(0, matches).map(listCard => listCard.copy(count = listCard.count + thisCard.count))
    val cardsNoNewCopies =
      remainingCards.slice(matches, remainingCards.size)
    thisCard.count + calculateScratchcardCount(cardsWinningNewCopy ::: cardsNoNewCopies)

def getFinalScratchcardCount(filename: String): Int = {
  val lines = Utils.fileLinesAs[List[String]](filename, _.toList)
  val scratchcards = lines.map(Scratchcard.fromLine)
  calculateScratchcardCount(scratchcards)
}

@main
def main(): Unit = {
  println(s"Part 1: ${getScratchcardsTotalPoints("inputs/4/input.txt")}")
  println(s"Part 2: ${getFinalScratchcardCount("inputs/4/input.txt")}")
}
