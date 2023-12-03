package com.oscarrobinson.adventofcode.utils

case class Point2D(x: Int, y: Int) {
  def getNeighbours(includeDiagonals: Boolean, xSize: Int, ySize: Int) = {
    val maxX = xSize - 1
    val maxY = ySize - 1
    List(
      // W Neighbour
      if (x > 0) Some(Point2D(x - 1, y)) else None,
      // NW Neighbour
      if (x > 0 && y > 0 && includeDiagonals) Some(Point2D(x - 1, y - 1)) else None,
      // N Neighbour
      if (y > 0) Some(Point2D(x, y - 1)) else None,
      // NE Neighbour
      if (y > 0 && x < maxX && includeDiagonals) Some(Point2D(x + 1, y - 1)) else None,
      // W Neighbour
      if (x < maxX) Some(Point2D(x + 1, y)) else None,
      // SW Neighbour
      if (x < maxX && y < maxY && includeDiagonals) Some(Point2D(x + 1, y + 1)) else None,
      // S Neighbour
      if (y < maxY) Some(Point2D(x, y + 1)) else None,
      // SW Neighbour
      if (x > 0 && y < maxY && includeDiagonals) Some(Point2D(x - 1, y + 1)) else None
    ).flatten
  }
}