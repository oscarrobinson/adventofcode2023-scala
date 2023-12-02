package com.oscarrobinson.adventofcode.second

import scala.io.Source

case class CubeCounts(red: Int, green: Int, blue: Int) {
  def isPossible(gameConfig: CubeCounts): Boolean = {
    red <= gameConfig.red && green <= gameConfig.green && blue <= gameConfig.blue
  }

  def powerset = red * green * blue
}

object CubeCounts {
  def fromRawCounts(rawText: String): CubeCounts = {
    val rawColourCounts = rawText.split(',').map(_.strip())
    val colourCountTuples = rawColourCounts.map(_.strip().split(' ').toList).map {
      case List(count, colour) => (count, colour)
      case invalid => throw new RuntimeException(s"Invalid cube record: ${invalid.mkString}")
    }
    CubeCounts(
      colourCountTuples.find(_._2 == "red").map(_._1.toInt).getOrElse(0),
      colourCountTuples.find(_._2 == "green").map(_._1.toInt).getOrElse(0),
      colourCountTuples.find(_._2 == "blue").map(_._1.toInt).getOrElse(0)
    )
  }
}

case class GameRecord(id: Int, rounds: Seq[CubeCounts]) {
  def isPossible(gameConfig: CubeCounts): Boolean = {
    rounds.forall(_.isPossible(gameConfig))
  }
  def minimalGameConfig: CubeCounts = {
    rounds.foldLeft(CubeCounts(0, 0, 0))((minimalConfig, roundCubeCounts) => {
      minimalConfig.copy(
        red = if (roundCubeCounts.red > minimalConfig.red) roundCubeCounts.red else minimalConfig.red,
        green = if (roundCubeCounts.green > minimalConfig.green) roundCubeCounts.green else minimalConfig.green,
        blue = if (roundCubeCounts.blue > minimalConfig.blue) roundCubeCounts.blue else minimalConfig.blue
      )
    })
  }
}

object GameRecord {
  def fromGameFileLine(rawLine: String): GameRecord = {
    val splitLine = rawLine.split(':')
    val id = splitLine.head.split(' ').last.strip().toInt
    val rawRoundsStr = splitLine.last
    val rounds = rawRoundsStr.split(';').map(CubeCounts.fromRawCounts)
    GameRecord(id, rounds)
  }
}

def getGameRecords(gameFilePath: String): List[GameRecord] = {
  val source = Source.fromFile(gameFilePath)
  try {
    source.getLines().map(GameRecord.fromGameFileLine).toList
  } finally {
    source.close()
  }
}
def sumPossibleGameIds(gameFilePath: String, gameConfig: CubeCounts): Int = {
  getGameRecords(gameFilePath).filter(_.isPossible(gameConfig)).map(_.id).sum
}

def sumPowerSetsOfMinPossibleGameConfigs(gameFilePath: String): Int = {
  getGameRecords(gameFilePath).map(_.minimalGameConfig.powerset).sum
}


@main
def main(): Unit = {
  val sum = sumPowerSetsOfMinPossibleGameConfigs("inputs/2/input.txt")
  println(sum)
}