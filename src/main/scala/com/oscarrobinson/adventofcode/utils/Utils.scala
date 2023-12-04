package com.oscarrobinson.adventofcode.utils

import scala.io.Source

object Utils {
  def fileLinesAs[T](filename: String, converter: Iterator[String] => T = itr => itr): T = {
    val source = Source.fromFile(filename)
    try {
      converter(source.getLines())
    } finally {
      source.close()
    }
  }
}
