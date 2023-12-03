package com.oscarrobinson.adventofcode.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Point2DTest extends AnyWordSpec with Matchers {

  "getNeighbours" should {
    "return correct neighbours when including diagonals and point is not next to an edge" in {
      /*
         01234
        0.....
        1.*...
        2.....
        3.....
        4.....
      */
      Point2D(1, 1).getNeighbours(true, 5, 5) should contain theSameElementsAs List(
        Point2D(0, 1),
        Point2D(0, 0),
        Point2D(1, 0),
        Point2D(2, 0),
        Point2D(2, 1),
        Point2D(2, 2),
        Point2D(1, 2),
        Point2D(0, 2),
      )
    }

    "return correct neighbours when not including diagonals and point is not next to an edge" in {
      /*
         01234
        0.....
        1.*...
        2.....
        3.....
        4.....
      */
      Point2D(1, 1).getNeighbours(false, 5, 5) should contain theSameElementsAs List(
        Point2D(0, 1),
        Point2D(1, 0),
        Point2D(2, 1),
        Point2D(1, 2)
      )
    }

    "return correct neighbours when including diagonals and point is next to top edge and left edge" in {
      /*
         01234
        0*....
        1.....
        2.....
        3.....
        4.....
      */
      Point2D(0, 0).getNeighbours(true, 5, 5) should contain theSameElementsAs List(
        Point2D(1, 0),
        Point2D(1, 1),
        Point2D(0, 1)
      )
    }

    "return correct neighbours when including diagonals and point is next to bottom edge and right edge" in {
      /*
         01234
        0.....
        1.....
        2.....
        3.....
        4....*
      */
      Point2D(4, 4).getNeighbours(true, 5, 5) should contain theSameElementsAs List(
        Point2D(3, 4),
        Point2D(3, 3),
        Point2D(4, 3)
      )
    }

    "return correct neighbours when not including diagonals and point is next to bottom edge and right edge" in {
      /*
         01234
        0.....
        1.....
        2.....
        3.....
        4....*
      */
      Point2D(4, 4).getNeighbours(false, 5, 5) should contain theSameElementsAs List(
        Point2D(3, 4),
        Point2D(4, 3)
      )
    }
  }
}
