package com.oscarrobinson.adventofcode.third

import com.oscarrobinson.adventofcode.utils.{CoordSpace, CoordSpaceValue}


case class SchematicNumber(points: List[CoordSpaceValue[Char]]) {
  def isEmpty = points.isEmpty

  def numericValue: Int = points.map(_.contents).mkString.toInt
}

case class Gear(part1: SchematicNumber, part2: SchematicNumber) {
  val ratio: Int = part1.numericValue * part2.numericValue
}

case class Schematic(coordSpace: CoordSpace[Char]) {

  def getPartNumbers(): Seq[SchematicNumber] = {
    getSchematicNumbers().filter(isPartNumber)
  }

  def getGears(): Seq[Gear] = coordSpace.allPoints
    .filter(_.contents == '*')
    .map(getNeighbouringSchematicNumbers)
    .filter(_.size == 2)
    .map(partList => Gear(partList(0), partList(1)))
  private def getNeighbouringSchematicNumbers(point: CoordSpaceValue[Char]) = {
    val neighbouringPoints = coordSpace.getNeighbours(point, includeDiagonals = true)
    val schematicNumbers = getSchematicNumbers()
    schematicNumbers.filter(schematicNumber => {
      schematicNumber.points.intersect(neighbouringPoints).nonEmpty
    })
  }

  private def getSchematicNumbers(): Seq[SchematicNumber] = {
    coordSpace.contents.flatMap(getSchematicNumbersFromRow)
  }

  private def getNeighboursOf(schematicNumber: SchematicNumber) =
    schematicNumber.points.flatMap(point => coordSpace.getNeighbours(point, includeDiagonals = true)).distinct

  private def isPartNumber(schematicNumber: SchematicNumber): Boolean =
    getNeighboursOf(schematicNumber).exists(coordSpaceValue =>
      !(coordSpaceValue.contents.isDigit || coordSpaceValue.contents == '.'))

  private def getSchematicNumbersFromRow(row: Array[CoordSpaceValue[Char]]): List[SchematicNumber] = {
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
}

object Schematic {
  def fromFileName(fileName: String): Schematic = {
    Schematic(CoordSpace.fromFile(fileName))
  }
}

def sumPartNumbers(schematic: Schematic): Int =
  schematic.getPartNumbers().map(_.numericValue).sum

def sumGearRatios(schematic: Schematic): Int =
  schematic.getGears().map(_.ratio).sum

@main
def main(): Unit = {
  val schematic = Schematic.fromFileName("inputs/3/input.txt")
  println(s"Part 1: ${sumPartNumbers(schematic)}")
  println(s"Part 2: ${sumGearRatios(schematic)}")
}