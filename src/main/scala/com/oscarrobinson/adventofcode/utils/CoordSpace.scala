package com.oscarrobinson.adventofcode.utils

case class CoordSpace[T](contents: Array[Array[CoordSpaceValue[T]]]) {
  val width: Int = contents.head.length
  val height: Int = contents.length
  
  val allPoints: Array[CoordSpaceValue[T]] = contents.flatten
  def getNeighbours(point: Point2D, includeDiagonals: Boolean) = point.getNeighbours(includeDiagonals, width, height)
  
  def getValue(point: Point2D): CoordSpaceValue[T] = contents(point.y)(point.x)
}
object CoordSpace {
  def fromFile(filename: String) = {
    val lines = Utils.fileLinesAs[Array[(String, Int)]](filename, _.toArray.zipWithIndex)
    val contents = lines.map { case (lineValue, yCoord) =>
      lineValue.toArray.zipWithIndex.map { case (value, xCoord) =>
        CoordSpaceValue(value, Point2D(xCoord, yCoord))
      }
    }
    CoordSpace(contents)
  }
}

case class CoordSpaceValue[T](contents: T, coordinates: Point2D)