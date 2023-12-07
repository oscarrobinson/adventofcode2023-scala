package com.oscarrobinson.adventofcode.seventh

import com.oscarrobinson.adventofcode.utils.Utils

case class Hand(cards: String, bid: Long) {
  val cardsScore = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse.zipWithIndex.toMap.view.mapValues(_ + 2).toMap

  val groupedHand = {
    cards
      .toList
      .groupBy((char: Char) => char)
      .values.toList
      .sortBy(groupList => {
        // Ensure sorted by largest group of cards first, then sort groups of same size by how high scoring the card is
        groupList.size * 100 + cardsScore(groupList.head)
      })
      .reverse
  }

  def typeRank(): Int = {
    groupedHand match
      // five of a kind
      case _ :: Nil => 6
      // four of a kind
      case group :: _ if group.size == 4 => 5
      // full house
      case firstGroup :: secondGroup :: Nil if firstGroup.size == 3 && secondGroup.size == 2 => 4
      // three of a kind
      case firstGroup :: _ if firstGroup.size == 3 => 3
      // two pair
      case firstGroup :: secondGroup :: _ if firstGroup.size == 2 && secondGroup.size == 2 => 2
      // one pair
      case firstGroup :: _ if firstGroup.size == 2 => 1
      // high card
      case _ => 0
  }
  def cardsRank(): Int = cards.map(cardsScore).map(num => "%02d".format(num)).mkString.toInt
  
  def totalRank(): Long = typeRank() * 100000000000L + cardsRank()
}

def daySevenPartOne(filename: String): Long = {
  val lines = Utils.fileLinesAs(filename, _.toList)
  val hands =  lines.map(_.split(' ')).map(list => Hand(list(0), list(1).toLong))
  hands.sortBy(_.totalRank()).zipWithIndex.foldLeft(0L) { case (totalWinnings, (hand, rank)) =>
    totalWinnings + (hand.bid * (rank + 1))
  }
}

def daySevenPartTwo(filename: String): Long = {
  0L
}

@main
def main(): Unit = {
  println(s"Part 1: ${daySevenPartOne("inputs/7/input.txt")}")
  println(s"Part 2: ${daySevenPartTwo("inputs/7/input.txt")}")
}