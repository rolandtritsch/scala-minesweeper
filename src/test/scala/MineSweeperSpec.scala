package minesweeper

import org.scalatest.{FlatSpec, Matchers}

class MineSweeperSpec extends FlatSpec with Matchers {
  import MineSweeper._

  "readFileIntoRawField" should "throw an exception, if the file does not exist" in {
    assertThrows[IllegalArgumentException] (readFileIntoRawField("./doesnotexist.txt"))
  }

  def readFileHelper(fileName: String): RawField = {
    readFileIntoRawField(getClass.getResource(fileName).getPath)
  }

  it should "throw an exception, if the file is malformed" in {
    assertThrows[AssertionError](readFileHelper("/bad-dim-negative.txt"))
    assertThrows[AssertionError](readFileHelper("/bad-dim-one.txt"))
    assertThrows[NumberFormatException](readFileHelper("/bad-dim-text.txt"))
    assertThrows[AssertionError](readFileHelper("/bad-dim-three.txt"))
    assertThrows[AssertionError](readFileHelper("/bad-empty.txt"))
    assertThrows[AssertionError](readFileHelper("/bad-field-chars.txt"))
    assertThrows[AssertionError](readFileHelper("/bad-not-enough-cols.txt"))
    assertThrows[AssertionError](readFileHelper("/bad-not-enough-rows.txt"))
  }

  it should "return a raw field, for a given file" in {
    readFileHelper("/1x1-mine.txt") should equal (RawField(1, 1, List('*')))
    readFileHelper("/1x1-save.txt") should equal (RawField(1, 1, List('.')))
    readFileHelper("/3x5-full.txt") should equal (RawField(3, 5, "***************".toList))
    readFileHelper("/6x4-save.txt") should equal (RawField(6, 4, "........................".toList))
    readFileHelper("/4x4-kata.txt") should equal (RawField(4, 4, "*........*......".toList))
    readFileHelper("/3x5-kata.txt") should equal (RawField(3, 5, "**.........*...".toList))
  }

  it should "return a num field, for a given raw field" in {
    readFileHelper("/1x1-mine.txt").toNumField should equal (NumField(1, 1, List(1)))
    readFileHelper("/1x1-save.txt").toNumField should equal (NumField(1, 1, List(0)))
    readFileHelper("/3x5-full.txt").toNumField should equal (NumField(3, 5, "111111111111111".toList.map(_.toString.toInt)))
    readFileHelper("/6x4-save.txt").toNumField should equal (NumField(6, 4, "000000000000000000000000".toList.map(_.toString.toInt)))
    readFileHelper("/4x4-kata.txt").toNumField should equal (NumField(4, 4, "1000000001000000".toList.map(_.toString.toInt)))
    readFileHelper("/3x5-kata.txt").toNumField should equal (NumField(3, 5, "110000000001000".toList.map(_.toString.toInt)))
  }


  it should "return an expanded num field, for a given num field" in {
    readFileHelper("/1x1-mine.txt").toNumField.expand should equal (ExpandedNumField(3, 3, List(0, 0, 0, 0, 1, 0, 0, 0, 0)))
    readFileHelper("/1x1-save.txt").toNumField.expand should equal (ExpandedNumField(3, 3, List(0, 0, 0, 0, 0, 0, 0, 0, 0)))
    readFileHelper("/3x5-full.txt").toNumField.expand should equal (ExpandedNumField(5, 7, "00000000111110011111001111100000000".toList.map(_.toString.toInt)))
  }

  "FieldIterator" should "return all squares of a field" in {
    val it = readFileHelper("/3x5-kata.txt").toNumField.iterator
    val itResult = (for(f <- it) yield f).toList
    val result = List(
      (0, 0, 1),
      (0, 1, 1),
      (0, 2, 0),
      (0, 3, 0),
      (0, 4, 0),
      (1, 0, 0),
      (1, 1, 0),
      (1, 2, 0),
      (1, 3, 0),
      (1, 4, 0),
      (2, 0, 0),
      (2, 1, 1),
      (2, 2, 0),
      (2, 3, 0),
      (2, 4, 0)
    )
    itResult should equal (result)
  }


}