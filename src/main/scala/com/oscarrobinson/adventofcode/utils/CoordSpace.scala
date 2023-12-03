package com.oscarrobinson.adventofcode.utils

case class CoordSpace[T](contents: Array[Array[CoordSpaceValue[T]]]) {
  val width: Int = contents.head.length
  val height: Int = contents.length

  val allPoints: Array[CoordSpaceValue[T]] = contents.flatten

  def getValue(point: Point2D): CoordSpaceValue[T] = contents(point.y)(point.x)

  def getNeighbours(point: CoordSpaceValue[T], includeDiagonals: Boolean): Seq[CoordSpaceValue[T]] = {
    point.coordinates.getNeighbours(includeDiagonals, width, height).map(getValue)
  }
}

object CoordSpace {
  def fromFile[T](filename: String, valueTransformer: Char => T = char => char): CoordSpace[T] = {
    val lines = Utils.fileLinesAs[Array[(String, Int)]](filename, _.toArray.zipWithIndex)
    val contents = lines.map { case (lineValue, yCoord) =>
      lineValue.toArray.zipWithIndex.map { case (char, xCoord) =>
        CoordSpaceValue(valueTransformer(char), Point2D(xCoord, yCoord))
      }
    }
    CoordSpace(contents)
  }
}

case class CoordSpaceValue[T](contents: T, coordinates: Point2D)