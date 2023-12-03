package com.oscarrobinson.adventofcode.third

import com.oscarrobinson.adventofcode.utils.{CoordSpace, CoordSpaceValue}


case class SchematicNumber(points: List[CoordSpaceValue[Char]]) {
  def isEmpty = points.isEmpty

  def numericValue: Int = points.map(_.contents).mkString.toInt

  def neighbouringPoints(schematic: CoordSpace[Char]) = {
    val neighbourCoords = points.flatMap(_.coordinates.getNeighbours(true, schematic.width, schematic.height)).distinct
    val numberCoords = points.map(_.coordinates)
    neighbourCoords.filterNot(coord => numberCoords.contains(coord))
  }

  def isPartNumber(schematic: CoordSpace[Char]): Boolean = {
    val neighbours = neighbouringPoints(schematic)
    neighbours.map(point => schematic.getValue(point)).exists(coordSpaceValue =>
      !(coordSpaceValue.contents.isDigit || coordSpaceValue.contents == '.'))
  }
}


def getSchematicNumbersFromRow(row: Array[CoordSpaceValue[Char]]): List[SchematicNumber] = {
  row.foldLeft(Nil: List[SchematicNumber]) {
    case (currentNumber :: restOfNumbers, point) if point.contents.isDigit =>
      currentNumber.copy(points = currentNumber.points :+ point) :: restOfNumbers
    case (Nil, point) if point.contents.isDigit =>
      List(SchematicNumber(List(point)))
    case (currentNumber :: numbers, point) if !currentNumber.isEmpty =>
      SchematicNumber(Nil) :: currentNumber :: numbers
    case (numbers, _) =>
      numbers
  }.filterNot(_.isEmpty).reverse
}

def getGearsFromRow(row: Array[CoordSpaceValue[Char]]): Array[CoordSpaceValue[Char]] = {
  row.filter(_.contents == '*')
}

def getGearNeighbouringNumbers(gear: CoordSpaceValue[Char], numbers: Seq[SchematicNumber], schematic: CoordSpace[Char]): Seq[SchematicNumber] = {
  numbers.filter(_.neighbouringPoints(schematic).contains(gear.coordinates))
}

def sumPartNumbers(schematicFileName: String): Int = {
  val schematic = CoordSpace.fromFile(schematicFileName)
  val numbersInSchematic = schematic.contents.flatMap(getSchematicNumbersFromRow)
  val partNumbersInSchematic = numbersInSchematic.filter(_.isPartNumber(schematic))
  partNumbersInSchematic.map(_.numericValue).sum
}

def sumGearRatios(schematicFileName: String): Int = {
  val schematic = CoordSpace.fromFile(schematicFileName)
  val numbersInSchematic = schematic.contents.flatMap(getSchematicNumbersFromRow)
  val gearsInSchematic = schematic.contents.flatMap(getGearsFromRow)
  val gearNumbers = gearsInSchematic
    .map(gear => getGearNeighbouringNumbers(gear, numbersInSchematic, schematic).toList)
    .filter(_.size == 2)
  gearNumbers.flatMap {
    case Seq(first, second) => Some(first.numericValue * second.numericValue)
    case _ => None
  }.sum
}


@main
def main(): Unit = {
  print(sumGearRatios("inputs/3/input.txt"))
}