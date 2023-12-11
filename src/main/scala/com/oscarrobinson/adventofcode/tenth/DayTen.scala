package com.oscarrobinson.adventofcode.tenth

import com.oscarrobinson.adventofcode.utils.{Grid, GridValue, Point2D, Utils}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class PipeGrid(val grid: Grid[Char]) {
  val start = grid.allGridValues.find(_.contents == 'S').get

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

  lazy val pathNodes = {
    val path = ListBuffer[GridValue[Char]](start)
    var currentNode = getNavigablePipes(start).head
    while(currentNode != start) {
      path.addOne(currentNode)
      currentNode = getNavigablePipes(currentNode).filterNot(path.contains).headOption.getOrElse(start)
    }
    path.toSet
  }

  lazy val nodesOutsidePath = {
    val outsidePathSet = mutable.Set.empty[GridValue[Char]]

    val outsideQueue = mutable.Queue(grid.allGridValues.head)

    while (outsideQueue.nonEmpty) {
      println(outsidePathSet.size)
      val node = outsideQueue.dequeue()
      outsidePathSet.add(node)
      grid.getNeighbours(node, includeDiagonals = false)
        .filterNot(neighbour => pathNodes.contains(neighbour) || outsideQueue.contains(neighbour) || outsidePathSet.contains(neighbour))
        .foreach(outsideQueue.enqueue)
    }

    outsidePathSet.toSet
  }

  def getNavigablePipes(position: GridValue[Char]): Seq[GridValue[Char]] =
    getNavigableFromPosition(position)
      .filter(neighbour => getNavigableFromPosition(neighbour).contains(position))

  def getNextPosition(location: GridValue[Char], prevLocation: GridValue[Char]) =
    getNavigablePipes(location).filterNot(_ == prevLocation).head

  def display() = {
    val ANSI_RESET = "\u001B[0m"
    val ANSI_RED = "\u001B[31m"
    val ANSI_BLUE = "\u001b[34m"
    for (row <- grid.contents) {
      for (cell <- row) {
        if (pathNodes.contains(cell)) {
          print(s"${ANSI_RED}${cell.contents}${ANSI_RESET}")
        } else if (nodesOutsidePath.contains(cell)) {
          print(s"${ANSI_BLUE}${cell.contents}${ANSI_RESET}")
        } else {
          print(cell.contents)
        }
      }
      print("\n")
    }
  }
}


def dayTenPartOne(filename: String): Long = {
  val grid = PipeGrid(Grid.fromFile(filename))
  val start = grid.grid.allGridValues.find(_.contents == 'S').get
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
  val initialGrid = PipeGrid(Grid.fromFile(filename))

  val simplifiedGrid = initialGrid.grid.contents.map(row => {
    row.map(value => {
      if (initialGrid.pathNodes.contains(value)) value.contents
      else ','
    })
  })

  val rawGridYPadded = simplifiedGrid.flatMap(rowAbove => {
    val padding = rowAbove.map { charAbove =>
      if (List('|', '7', 'F', 'S').contains(charAbove)) '|' else '.'
    }
    List(rowAbove, padding)
  })

  val gridXYPadded = rawGridYPadded.map(row => {
    val paddedRow = row.flatMap(cellLeft => {
     val padding =  if (List('-', 'L', 'F', 'S').contains(cellLeft)) '-' else '.'
      List(cellLeft, padding)
    })
    // Add final padding to end and start of row so we know path has a border and can flood fill properly
    '.' +: paddedRow :+ '.'
  }).map(_.toArray)

  val paddingRow = Range(0, gridXYPadded.head.length).map(_ => '.').toArray

  val grid = PipeGrid(Grid.fromArray(paddingRow +: gridXYPadded :+ paddingRow))

  grid.display()

  grid.grid.allGridValues.filterNot(gridValue => {
    grid.nodesOutsidePath.contains(gridValue) ||
      grid.pathNodes.contains(gridValue) ||
      gridValue.contents == '.'
  }).size.toLong
}

@main
def main(): Unit = {
  println(s"Part 1: ${dayTenPartOne("inputs/10/input.txt")}")
  println(s"Part 2: ${dayTenPartTwo("inputs/10/input.txt")}")
}
