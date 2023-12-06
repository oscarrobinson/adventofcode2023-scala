package com.oscarrobinson.adventofcode.sixth

import com.oscarrobinson.adventofcode.utils.Utils

case class Race(raceTime: Long, recordDistance: Long) {
  def calculateDistance(buttonHoldTime: Long) = {
    if (raceTime - buttonHoldTime <= 0) 0
    else (raceTime - buttonHoldTime) * buttonHoldTime
  }

  def getNumNewRecordDistancesPossible(): Long =
    Range.Long(0, raceTime, 1).foldLeft(0L){ case (recordRacesCount, buttonHoldTime) =>
      val raceDistance = calculateDistance(buttonHoldTime)
      if (raceDistance > recordDistance) recordRacesCount + 1
      else recordRacesCount
    }
}

def parseNumbers(line: String): Array[String] =
  line.split(':')(1).split("""\s+""").filter(_.nonEmpty)
def daySixPartOne(filename: String): Long = {
  val raceData = Utils.fileLinesAs(filename, _.toList)
  val times = parseNumbers(raceData.head).map(_.toLong)
  val distances = parseNumbers(raceData(1)).map(_.toLong)
  val races = times.zip(distances).map{ (time, distance) => Race(time, distance) }
  races.map(_.getNumNewRecordDistancesPossible()).product
}

def daySixPartTwo(filename: String): Long = {
  val raceData = Utils.fileLinesAs(filename, _.toList)
  val time = parseNumbers(raceData.head).mkString.toLong
  val distance = parseNumbers(raceData(1)).mkString.toLong
  Race(time, distance).getNumNewRecordDistancesPossible()
}

@main
def main(): Unit = {
  println(s"Part 1: ${daySixPartOne("inputs/6/input.txt")}")
  println(s"Part 2: ${daySixPartTwo("inputs/6/input.txt")}")
}