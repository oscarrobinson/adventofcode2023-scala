package com.oscarrobinson.adventofcode.eighth

import com.oscarrobinson.adventofcode.utils.Utils

import scala.annotation.tailrec


case class GhostDesertMap(rawDirections: String, mapLines: List[String]) {
  val directions = rawDirections.toList

  val directionsIterator = LazyList.continually(directions).flatten

  val map = {
    mapLines.map(line => {
      val nodeAddr = line.split('=').head.strip()
      val left = line.split('=').last.split(',').head.filterNot(_ == '(').strip()
      val right = line.split('=').last.split(',').last.filterNot(_ == ')').strip()
      (nodeAddr, Map(('L', left), ('R', right)))
    }).toMap
  }

  def stepsFromStartingLocationToFinish(startingLocation: String): Long = {
    var stepsTaken = 0L
    var currentLocation = startingLocation
    directionsIterator.takeWhile(_ => currentLocation.last != 'Z').foreach(direction => {
      currentLocation = map(currentLocation)(direction)
      stepsTaken += 1
    })
    stepsTaken
  }

  @tailrec
  private def gcd(a: Long, b: Long): Long = b match {
    case 0L => a
    case _ => gcd(b, a % b)
  }

  private def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))

  private def lcm(numList: List[Long]): Long = numList match {
    case Nil => 0L
    case num :: Nil => num
    case num :: rest => lcm(num, lcm(rest))
  }

  def steps: Long = {
    val startingLocations = map.keys.filter(_.last == 'A').toList
    // Every path must loop, so first get num steps from start to finish for every starting location
    val stepsToFinish = startingLocations.map(stepsFromStartingLocationToFinish)
    // We then need to work out how many steps til all journeys are on the finish simultaneously
    // This will be the lowest common multiple of the total steps for each individual journey
    lcm(stepsToFinish)
  }

}

case class DesertMap(rawDirections: String, mapLines: List[String]) {
  val directions = rawDirections.toList

  val directionsIterator = LazyList.continually(directions).flatten

  val map = {
    mapLines.map(line => {
      val nodeAddr = line.split('=').head.strip()
      val left = line.split('=').last.split(',').head.filterNot(_ == '(').strip()
      val right = line.split('=').last.split(',').last.filterNot(_ == ')').strip()
      (nodeAddr, Map(('L', left), ('R', right)))
    }).toMap
  }

  def steps: Long = {
    var stepsTaken = 0L
    var currentLocation = "AAA"
    directionsIterator.takeWhile(_ => currentLocation != "ZZZ").foreach(direction => {
      currentLocation = map(currentLocation)(direction)
      stepsTaken += 1
    })
    stepsTaken
  }

}


def dayEightPartOne(filename: String): Long = {
  val fileLines = Utils.fileLinesAs(filename, _.toList)
  val desertMap = DesertMap(fileLines.head, fileLines.tail.filter(_.nonEmpty))
  println(desertMap.map)
  desertMap.steps
}

def dayEightPartTwo(filename: String): Long = {
  val fileLines = Utils.fileLinesAs(filename, _.toList)
  val desertMap = GhostDesertMap(fileLines.head, fileLines.tail.filter(_.nonEmpty))
  desertMap.steps
}

@main
def main(): Unit = {
  println(s"Part 1: ${dayEightPartOne("inputs/8/input.txt")}")
  println(s"Part 2: ${dayEightPartTwo("inputs/8/input.txt")}")
}