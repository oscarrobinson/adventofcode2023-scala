package com.oscarrobinson.adventofcode.fifth

import com.oscarrobinson.adventofcode.utils.Utils

case class AlmanacRange(sourceStart: Long, destinationStart: Long, length: Long) {
  def convert(sourceValue: Long): Option[Long] =
    if (sourceValue >= sourceStart && sourceValue < (sourceStart + length))
      Some(sourceValue - (sourceStart - destinationStart))
    else None
}

object AlmanacRange {
  def fromLine(line: String): AlmanacRange = {
    val splitLine = line.split("""\s+""").filterNot(_.isEmpty)
    AlmanacRange(splitLine(1).toLong, splitLine(0).toLong, splitLine(2).toLong)
  }
}

def getLowestLocationNumber(filename: String): Long = {
  val lines = Utils.fileLinesAs(filename, _.toList)
  val seeds = lines(0).split(':').last.split("""\s+""").filterNot(_.isEmpty).map(_.toLong)
  val rangesList = lines.tail.foldLeft(Vector(): Vector[Vector[AlmanacRange]])((rangeLists, line) => {
    if (line.isBlank) {
      rangeLists
    } else if (!line(0).isDigit) {
      rangeLists :+ Vector()
    } else {
      rangeLists.slice(0, rangeLists.size - 1) :+ (rangeLists(rangeLists.size - 1) :+ AlmanacRange.fromLine(line))
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

def getLowestLocationNumberPart2(filename: String): Long = {
  val lines = Utils.fileLinesAs(filename, _.toList)
  val seedRanges = lines.head.split(':').last.split("""\s+""").filterNot(_.isEmpty).map(_.toLong).grouped(2).toList
    .map(range => {
      Range.Long.apply(range(0), range(0) + range(1), 1)
    })

  val rangesList = lines.tail.foldLeft(Vector(): Vector[Vector[AlmanacRange]])((rangeLists, line) => {
    if (line.isBlank) {
      rangeLists
    } else if (!line(0).isDigit) {
      rangeLists :+ Vector()
    } else {
      rangeLists.slice(0, rangeLists.size - 1) :+ (rangeLists(rangeLists.size - 1) :+ AlmanacRange.fromLine(line))
    }
  })

  // Brute force cos not enough time to do a nice optimal solution today
  val minLocations = seedRanges.zipWithIndex.map { case (seedRange, index) =>
    println(s"Range ${index + 1}")
    seedRange.foldLeft(Long.MaxValue) { case (minLocation, seed) =>
      val location = rangesList.foldLeft(seed)((value, ranges) => {
        val newValue = ranges.flatMap(_.convert(value)).lift(0)
        newValue.getOrElse(value)
      })
      if (location < minLocation) location else minLocation
    }
  }

  minLocations.min
}
@main
def main(): Unit = {
  println(s"Part 1: ${getLowestLocationNumber("inputs/5/input.txt")}")
  println(s"Part 2: ${getLowestLocationNumberPart2("inputs/5/input.txt")}")
}
