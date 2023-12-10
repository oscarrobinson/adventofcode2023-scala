package com.oscarrobinson.adventofcode.utils

case class GridValue[T](contents: T, coordinates: Point2D)
case class Grid[T](contents: Array[Array[GridValue[T]]]) {
  val width: Int = contents.head.length
  val height: Int = contents.length

  val allGridValues: Array[GridValue[T]] = contents.flatten

  def getValue(point: Point2D): GridValue[T] = contents(point.y)(point.x)

  def getNeighbours(gridValue: GridValue[T], includeDiagonals: Boolean): Seq[GridValue[T]] = {
    gridValue.coordinates.getNeighbours(includeDiagonals, width, height).map(getValue)
  }
}

object Grid {
  def fromFile[T](filename: String, valueTransformer: Char => T = char => char): Grid[T] = {
    val lines = Utils.fileLinesAs[Array[(String, Int)]](filename, _.toArray.zipWithIndex)
    val contents = lines.map { case (lineValue, yCoord) =>
      lineValue.toArray.zipWithIndex.map { case (char, xCoord) =>
        GridValue(valueTransformer(char), Point2D(xCoord, yCoord))
      }
    }
    Grid(contents)
  }

  def fromArray[T](lists: Array[Array[T]]): Grid[T] = {
    val contents = lists.zipWithIndex.map { case (lineValue, yCoord) =>
      lineValue.zipWithIndex.map { case (char, xCoord) =>
        GridValue(char, Point2D(xCoord, yCoord))
      }
    }
    Grid(contents)
  }
}