package com.oscarrobinson.adventofcode.ninth

import com.oscarrobinson.adventofcode.utils.Utils

def getDifferences(sequence: List[Long]): List[List[Long]] = {
  val differences = sequence.sliding(2).map { case elem1 :: elem2 :: _ => elem2 - elem1 }.toList
  if (differences.forall(_ == 0L)) List(differences)
  else differences :: getDifferences(differences)
}

def getNext(sequence: List[Long]): Long = {
  val differences = sequence :: getDifferences(sequence)
  differences.reverse.foldLeft(0L)((prevSeqNextValue, nextDifference) => {
    prevSeqNextValue + nextDifference.last
  })
}

def getPrior(sequence: List[Long]): Long = {
  val differences = sequence :: getDifferences(sequence)
  differences.reverse.foldLeft(0L)((prevSeqPriorValue, nextDifference) => {
    nextDifference.head - prevSeqPriorValue
  })
}

def parseSequences(filename: String): List[List[Long]] =
  Utils.fileLinesAs(filename, _.toList)
    .map(_.split(' ').filter(_.nonEmpty).map(_.strip().toLong).toList)

def dayNinePartOne(filename: String): Long =
  parseSequences(filename).map(getNext).sum

def dayNinePartTwo(filename: String): Long =
  parseSequences(filename).map(getPrior).sum

@main
def main(): Unit = {
  println(s"Part 1: ${dayNinePartOne("inputs/9/input.txt")}")
  println(s"Part 2: ${dayNinePartTwo("inputs/9/input.txt")}")
}