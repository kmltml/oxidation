package oxidation.parse

class Sourcefile(val newlineIndexes: Vector[Int]) {

  def findLine(index: Int): Int = newlineIndexes.lastIndexWhere(_ <= index)

  def findColumn(line: Int, index: Int): Int = index - newlineIndexes(line) - 1

  def findLocation(index: Int): (Int, Int) = {
    val line = findLine(index)
    val column = findColumn(line, index)
    (line, column)
  }

}

object Sourcefile {

  def apply(src: Iterator[Char]): Sourcefile = {
    val newlineIndices = src.zipWithIndex.collect {
      case ('\n', i) => i
    }
    new Sourcefile(-1 +: newlineIndices.toVector)
  }

}
