package com.oscarrobinson.adventofcode.third

import com.oscarrobinson.adventofcode.utils.CoordSpace
import com.oscarrobinson.adventofcode.utils.Utils.fileLinesAs

import scala.io.Source



def sumPartNumbers(schematicFileName: String): Int = {
  val schematic = CoordSpace.fromFile(schematicFileName)

  schematic.contents.foreach(_.foreach(println))
  0
}


@main
def main(): Unit = {
  print(sumPartNumbers("inputs/3/input.txt"))
}