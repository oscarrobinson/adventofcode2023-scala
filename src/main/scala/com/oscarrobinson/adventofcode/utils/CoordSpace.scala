package com.oscarrobinson.adventofcode.utils

case class CoordSpace[T](width: Int, height: Int, contents: Array[Array[CoordSpaceValue[T]]])
object CoordSpace {
  def fromFile(filename: String) = {
    val lines = Utils.fileLinesAs[Array[(String, Int)]](filename, _.toArray.zipWithIndex)
    val contents = lines.map { case (lineValue, yCoord) =>
      lineValue.toArray.zipWithIndex.map { case (value, xCoord) =>
        CoordSpaceValue(value, Point2D(xCoord, yCoord))
      }
    }
    CoordSpace(contents.head.length, contents.length, contents)
  }
}

case class CoordSpaceValue[T](value: T, coordinates: Point2D)