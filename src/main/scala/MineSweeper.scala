package minesweeper

object MineSweeper {
  import scala.collection.AbstractIterator

  class FieldIterator[T](f: Field[T]) extends AbstractIterator[(Int, Int, T)] {
    var currentIndex = 0

    override def hasNext: Boolean = currentIndex < f.fields.size
    override def next: (Int, Int, T) = {
      val currentRow = currentIndex / f.cols
      val currentCol = currentIndex % f.cols
      val currentField = f.fields(currentIndex)
      currentIndex = currentIndex + 1
      (currentRow, currentCol, currentField)
    }
  }

  abstract class Field[T](val rows: Int, val cols: Int, val fields: List[T]) {
    def iterator = new FieldIterator[T](this)

    def apply(row: Int, col: Int): T = {
      fields((row * cols) + col)
    }
  }

  case class RawField(override val rows: Int, override val cols: Int, override val fields: List[Char]) extends Field[Char](rows, cols, fields) {
    def toNumField: NumField = {
      val numFields = fields.map {
        case '.' => 0
        case '*' => 1
      }
      NumField(rows, cols, numFields)
    }
  }

  case class NumField(override val rows: Int, override val cols: Int, override val fields: List[Int]) extends Field[Int](rows, cols, fields) {
    def expand: ExpandedNumField = {
      val expandedRows = rows + 2
      val expandedCols = cols + 2
      val iteratorFields = List.range(0, expandedRows * expandedCols)
      val expandedFields = iteratorFields.map {
        case i if i < expandedCols => 0
        case i if i >= (expandedRows - 1) * expandedCols => 0
        case i if (i + 1) % expandedCols == 0 => 0
        case i if i % expandedCols == 0 => 0
        case i => {
          val r = (i / expandedCols) - 1
          val c = (i % expandedCols) - 1
          fields((r * cols) + c)
        }
      }
      ExpandedNumField(expandedRows, expandedCols, expandedFields)
    }
  }

  case class ExpandedNumField(override val rows: Int, override val cols: Int, override val fields: List[Int]) extends Field[Int](rows, cols, fields) {
  }

  /** Read a file of the format ...
    *
    * 4 4
    * ..*.
    * .*..
    * **..
    * ....
    *
    * ... and return a RawField.
    *
    * @param fileName The name of the file to parse
    * @return The RawField
    */
  def readFileIntoRawField(fileName: String): RawField = {
    require(new java.io.File(fileName).exists)

    val lines = scala.io.Source.fromFile(fileName).getLines.toList
    assert(lines.nonEmpty, "lines.nonEmpty violated")

    val dimensions = lines.head.split(" ")
    assert(dimensions.size == 2, "dimensions.size == 2 violated")

    // this will raise an IllegalNumber exception, if the string is not parsable into an Int
    val (rows, cols) = (dimensions(0).toInt, dimensions(1).toInt)
    assert(rows > 0 && cols > 0, "rows > 0 && cols > 0 violated")

    val fieldLines = lines.tail
    assert(fieldLines.size == rows, "fieldLines.size == rows violated")

    val fields = fieldLines.map {l => {
      assert(l.size == cols, "l.size == cols violated")
      l.toList
    }}.flatten

    RawField(rows, cols, fields)
  } ensuring(rf =>
    rf.cols * rf.rows == rf.fields.size &&
    rf.fields.forall(".*".contains(_))
    )
}