package com.oscarrobinson.adventofcode.fifth

import com.oscarrobinson.adventofcode.utils.Utils

case class AlmanacRange(sourceStart: Double, destinationStart: Double, length: Double) {
  def convert(sourceValue: Double): Option[Double] =
    if (sourceValue >= sourceStart && sourceValue < (sourceStart + length))
      Some(sourceValue - (sourceStart - destinationStart))
    else None
}

object AlmanacRange {
  def fromLine(line: String): AlmanacRange = {
    val splitLine = line.split("""\s+""").filterNot(_.isEmpty)
    AlmanacRange(splitLine(1).toDouble, splitLine(0).toDouble, splitLine(2).toDouble)
  }
}

def getLowestLocationNumber(filename: String): Double = {
  val lines = Utils.fileLinesAs(filename, _.toList)
  val seeds = lines(0).split(':').last.split("""\s+""").filterNot(_.isEmpty).map(_.toDouble)
  val rangesList = lines.tail.foldLeft(Vector(): Vector[Vector[AlmanacRange]])((rangeLists, line) => {
    if (line.isBlank) {
      rangeLists
    } else if (!line(0).isDigit) {
      rangeLists :+ Vector()
    } else {
      rangeLists.slice(0, rangeLists.size - 1) :+ (rangeLists(rangeLists.size -1) :+ AlmanacRange.fromLine(line) )
    }
  })

  val seedLocations = seeds.map(seed => {
    rangesList.foldLeft(seed)((value, ranges) => {
      val newValue = ranges.flatMap(_.convert(value)).lift(0)
      newValue.getOrElse(value)
    })
  })

  seedLocations.min
}

@main
def main(): Unit = {
  println(s"Part 1: ${getLowestLocationNumber("inputs/5/input.txt")}")
}
