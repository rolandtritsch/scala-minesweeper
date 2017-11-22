package minesweeper

object MineSweeper {

  abstract class Field[T](val rows: Int, val cols: Int, val fields: List[List[T]])

  case class RawField(override val rows: Int, override val cols: Int, override val fields: List[List[Char]]) extends Field[Char](rows, cols, fields) {
    def toNumField: NumField = {
      val numFields = fields.map {_.map {
        case '.' => 0
        case '*' => 1
      }}
      NumField(rows, cols, numFields)
    }
  }

  case class NumField(override val rows: Int, override val cols: Int, override val fields: List[List[Int]]) extends Field[Int](rows, cols, fields) {
    def expand: ExpandedNumField = {
      val expandedRows = rows + 2
      val expandedCols = cols + 2
      val expandedFields = (for {
        r <- 0 to expandedRows - 1
        c <- 0 to expandedCols - 1
      } yield {
        if(r == 0) 0
        else if(r == expandedRows - 1) 0
        else if(c == 0) 0
        else if(c == expandedCols - 1) 0
        else fields(r - 1)(c - 1)
      }).toList.grouped(expandedCols).toList

      ExpandedNumField(expandedRows, expandedCols, expandedFields)
    }
  }

  case class ExpandedNumField(override val rows: Int, override val cols: Int, override val fields: List[List[Int]]) extends Field[Int](rows, cols, fields) {
    def count: CountField = {
      val countRows = rows - 2
      val countCols = cols - 2
      val countSquares = (for {
        r <- 1 to countRows
        c <- 1 to countCols
      } yield {
        List(
          fields(r - 1)(c - 1),
          fields(r - 1)(c),
          fields(r - 1)(c + 1),
          fields(r)(c - 1),
          fields(r)(c),
          fields(r)(c + 1),
          fields(r + 1)(c - 1),
          fields(r + 1)(c),
          fields(r + 1)(c + 1)
        )
      }).toList
      val countFields = countSquares.map(_.sum).grouped(countCols).toList
      CountField(countRows, countCols, countFields)
    }
  }

  case class CountField(override val rows: Int, override val cols: Int, override val fields: List[List[Int]]) extends Field[Int](rows, cols, fields) {
    def clue(mineField: RawField): RawField = {
      require(mineField.rows == rows && mineField.cols == cols, "mineField.rows == rows && mineField.cols == cols violated")
      val clueField = (for {
        r <- 0 to mineField.rows - 1
        c <- 0 to mineField.cols - 1
      } yield {
        mineField.fields(r)(c) match {
          case '*' => '*'
          case '.' => fields(r)(c).toString.charAt(0)
        }
      }).toList.grouped(cols).toList
      RawField(rows, cols, clueField)
    }
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
      val fs = l.toList
      assert(fs.forall(".*".contains(_)), "Invalid char found")
      fs
    }}

    RawField(rows, cols, fields)
  } ensuring(rf => rf.fields.size == rf.rows && rf.fields.head.size == rf.cols)

  def main(args: Array[String]): Unit = {
    require(args.size == 1, "Usage: MineSweeper <fileName>")

    val fileName = args(0)
    val rawField = readFileIntoRawField(fileName)
    val countField = rawField.toNumField.expand.count
    val clueField = countField.clue(rawField)

    clueField.fields.foreach(l => println(l.mkString))
  }
}