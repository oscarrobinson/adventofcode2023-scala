package com.oscarrobinson.adventofcode.third

import com.oscarrobinson.adventofcode.utils.{Grid, GridValue}


case class SchematicNumber(gridValues: List[GridValue[Char]]) {
  def isEmpty = gridValues.isEmpty

  def numericValue: Int = gridValues.map(_.contents).mkString.toInt
}

case class Gear(part1: SchematicNumber, part2: SchematicNumber) {
  val ratio: Int = part1.numericValue * part2.numericValue
}

case class Schematic(grid: Grid[Char]) {

  def getPartNumbers(): Seq[SchematicNumber] = {
    getSchematicNumbers().filter(isPartNumber)
  }

  def getGears(): Seq[Gear] = grid.allGridValues
    .filter(_.contents == '*')
    .map(getNeighbouringSchematicNumbers)
    .filter(_.size == 2)
    .map(partList => Gear(partList(0), partList(1)))
  private def getNeighbouringSchematicNumbers(gridValue: GridValue[Char]) = {
    val neighbouringGridValues = grid.getNeighbours(gridValue, includeDiagonals = true)
    val schematicNumbers = getSchematicNumbers()
    schematicNumbers.filter(schematicNumber => {
      schematicNumber.gridValues.intersect(neighbouringGridValues).nonEmpty
    })
  }

  private def getSchematicNumbers(): Seq[SchematicNumber] = {
    grid.contents.flatMap(getSchematicNumbersFromRow)
  }

  private def getNeighboursOf(schematicNumber: SchematicNumber) =
    schematicNumber.gridValues.flatMap(gridValue => grid.getNeighbours(gridValue, includeDiagonals = true)).distinct

  private def isPartNumber(schematicNumber: SchematicNumber): Boolean =
    getNeighboursOf(schematicNumber).exists(gridValue =>
      !(gridValue.contents.isDigit || gridValue.contents == '.'))

  private def getSchematicNumbersFromRow(row: Array[GridValue[Char]]): List[SchematicNumber] = {
    row.foldLeft(Nil: List[SchematicNumber]) {
      case (currentNumber :: restOfNumbers, gridValue) if gridValue.contents.isDigit =>
        currentNumber.copy(gridValues = currentNumber.gridValues :+ gridValue) :: restOfNumbers
      case (Nil, gridValue) if gridValue.contents.isDigit =>
        List(SchematicNumber(List(gridValue)))
      case (currentNumber :: numbers, _) if !currentNumber.isEmpty =>
        SchematicNumber(Nil) :: currentNumber :: numbers
      case (numbers, _) =>
        numbers
    }.filterNot(_.isEmpty).reverse
  }
}

object Schematic {
  def fromFileName(fileName: String): Schematic = {
    Schematic(Grid.fromFile(fileName))
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