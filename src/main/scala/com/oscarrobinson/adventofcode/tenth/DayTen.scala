package com.oscarrobinson.adventofcode.tenth

import com.oscarrobinson.adventofcode.utils.{Grid, Utils, GridValue, Point2D}

implicit class PipeGrid(grid: Grid[Char]) {
  private def getNavigableFromPosition(pos: GridValue[Char]): Seq[GridValue[Char]] = pos.contents match {
    case 'S' => grid.getNeighbours(pos, false)
    case '|' => grid.getNeighbours(pos, false)
      .filter(neighbour => List(pos.coordinates.y - 1, pos.coordinates.y + 1).contains(neighbour.coordinates.y))
    case '-' => grid.getNeighbours(pos, false)
      .filter(neighbour => List(pos.coordinates.x - 1, pos.coordinates.x + 1).contains(neighbour.coordinates.x))
    case 'L' => grid.getNeighbours(pos, false)
      .filter(neighbour => List(
        Point2D(pos.coordinates.x, pos.coordinates.y - 1),
        Point2D(pos.coordinates.x + 1, pos.coordinates.y)
      ).contains(neighbour.coordinates))
    case 'J' => grid.getNeighbours(pos, false)
      .filter(neighbour => List(
        Point2D(pos.coordinates.x, pos.coordinates.y - 1),
        Point2D(pos.coordinates.x - 1, pos.coordinates.y)
      ).contains(neighbour.coordinates))
    case '7' => grid.getNeighbours(pos, false)
      .filter(neighbour => List(
        Point2D(pos.coordinates.x, pos.coordinates.y + 1),
        Point2D(pos.coordinates.x - 1, pos.coordinates.y)
      ).contains(neighbour.coordinates))
    case 'F' => grid.getNeighbours(pos, false)
      .filter(neighbour => List(
        Point2D(pos.coordinates.x, pos.coordinates.y + 1),
        Point2D(pos.coordinates.x + 1, pos.coordinates.y)
      ).contains(neighbour.coordinates))
    case _ => Nil
  }

  def getNavigablePipes(position: GridValue[Char]): Seq[GridValue[Char]] =
    getNavigableFromPosition(position)
      .filter(neighbour => getNavigableFromPosition(neighbour).contains(position))

  def getNextPosition(location: GridValue[Char], prevLocation: GridValue[Char]) =
    getNavigablePipes(location).filterNot(_ == prevLocation).head
}


def dayTenPartOne(filename: String): Long = {
  val grid = Grid.fromFile(filename)
  val start = grid.allGridValues.find(_.contents == 'S').get
  val navigablePipesFromStart = grid.getNavigablePipes(start)

  var locationOne = navigablePipesFromStart(0)
  var locationTwo = navigablePipesFromStart(1)
  var prevLocationOne = start
  var prevLocationTwo = start
  var steps = 1

  while (locationOne.coordinates != locationTwo.coordinates) {
    val nextPrevLocationOne = locationOne
    val nextPrevLocationTwo = locationTwo
    locationOne = grid.getNextPosition(locationOne, prevLocationOne)
    locationTwo = grid.getNextPosition(locationTwo, prevLocationTwo)
    prevLocationOne = nextPrevLocationOne
    prevLocationTwo = nextPrevLocationTwo
    steps += 1
  }

  steps
}

def dayTenPartTwo(filename: String): Long = {
  0L
}

@main
def main(): Unit = {
  println(s"Part 1: ${dayTenPartOne("inputs/10/input.txt")}")
  println(s"Part 2: ${dayTenPartTwo("inputs/10/input.txt")}")
}
