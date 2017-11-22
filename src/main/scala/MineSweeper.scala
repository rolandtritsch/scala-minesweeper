package minesweeper

/** Implements a solution to the [[http://codingdojo.org/kata/Minesweeper CoderDojo Minesweeper Kata]].
  *
  * It reads a minefield from a file and then builds the clues from it
  * (the number of mines around a given field).
  *
  * I am going about it in a slightly unorthodox way, by transforming
  * the orginal field into a field of numbers.
  *
  * I then expand that field by putting zeros around it, because that
  * allows me to traverse the field and just select the 8 fields that
  * are around a field (even if the field is in a corner or at the edge
  * of the field).
  *
  * I then just some up the 8 fields (plus the field in the middle) and
  * I am done.
  */
object MineSweeper {

  /** The base class for all kinds/types of fields.
    *
    * @param rows The number of rows
    * @param cols The number of cols
    * @param fields The field
    * @tparam T The type of the field (can be Char or Int)
    */
  abstract class Field[T](val rows: Int, val cols: Int, val fields: List[List[T]])

  /** The [[RawField]]
    *
    * {{{
    * 4 4
    * *...
    * ....
    * .*..
    * ....
    * }}}
    */
  case class RawField(override val rows: Int, override val cols: Int, override val fields: List[List[Char]]) extends Field[Char](rows, cols, fields) {

    /** @return The [[NumField]] for this [[RawField]]
      */
    def toNumField: NumField = {
      val numFields = fields.map {_.map {
        case '.' => 0
        case '*' => 1
      }}
      NumField(rows, cols, numFields)
    }
  }

  /** The [[NumField]].
    *
    * {{{
    * 4 4
    * 1000
    * 0000
    * 0100
    * 0000
    * }}}
    *
    * This field shows where the mines are in numerical format.
    *
    * We need this later on to count the mines in a square.
    */
  case class NumField(override val rows: Int, override val cols: Int, override val fields: List[List[Int]]) extends Field[Int](rows, cols, fields) {

    /** @return The [[ExpandedNumField]] for this [[NumField]]
      */
    def toExpNumField: ExpandedNumField = {
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

  /** The expanded num field.
    *
    * {{{
    * 6 6
    *  '000000'
    * '0'1000'0'
    * '0'0000'0'
    * '0'0100'0'
    * '0'0000'0'
    *  '000000'
    * }}}
    *
    * Surround the original field with zeros. This will allow us to count all mines in all
    * squares easily (without having to worry about the edges).
    */
  case class ExpandedNumField(override val rows: Int, override val cols: Int, override val fields: List[List[Int]]) extends Field[Int](rows, cols, fields) {

    /** Building the [[CountField]].
      *
      * Here comes the trick.
      *
      * On an [[ExpandedNumField]] we can just walk over the original [[NumField]] ...
      *
      * {{{
      * 6 6
      *  000000
      * 0'1000'0
      * 0'0000'0
      * 0'0100'0
      * 0'0000'0
      *  000000
      * }}}
      *
      * ... and can build squares of 9 numbers ...
      *
      * {{{
      * 6 6
      * '000'000
      * '010'000
      * '000'000
      * 001000
      * 000000
      * 000000
      * }}}
      *
      * ... that can then be sumed up to calc the clue for a given field.
      *
      * @return The [[CountField]] for this [[ExpandedNumField]]
      */
    def toCountField: CountField = {
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

  /** The [[CountField]]
    *
    * {{{
    * 4 4
    * 1100
    * 2210
    * 1110
    * 1110
    * }}}
    */
  case class CountField(override val rows: Int, override val cols: Int, override val fields: List[List[Int]]) extends Field[Int](rows, cols, fields) {
    /** Construct the field with all of the clues.
      *
      * Use the [[CountField]] with the given mineField to construct the field with the clues.
      *
      * {{{
      * 4 4
      * *100
      * 2210
      * 1*10
      * 1110
      * }}}
      *
      * @param mineField The original mine field with the mine locations
      * @return The [[RawField]] with all of the calculated clues
      */
    def toClueField(mineField: RawField): RawField = {
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
    * {{{
    * 4 4
    * ..*.
    * .*..
    * **..
    * ....
    * }}}
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

  /** The [[main]] function.
    *
    * @param args The filename to process.
    */
  def main(args: Array[String]): Unit = {
    require(args.size == 1, "Usage: MineSweeper <fileName>")

    val fileName = args(0)
    val rawField = readFileIntoRawField(fileName)
    val countField = rawField.toNumField.toExpNumField.toCountField
    val clueField = countField.toClueField(rawField)

    clueField.fields.foreach(l => println(l.mkString))
  }
}