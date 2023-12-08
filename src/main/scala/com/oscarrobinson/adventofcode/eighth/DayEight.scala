package com.oscarrobinson.adventofcode.eighth

import com.oscarrobinson.adventofcode.utils.Utils


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
  val desertMap = DesertMap(fileLines.head, fileLines.tail.filter(_.nonEmpty))
}

@main
def main(): Unit = {
  println(s"Part 1: ${dayEightPartOne("inputs/8/input.txt")}")
  println(s"Part 2: ${dayEightPartTwo("inputs/8/input.txt")}")
}