/*
 * This file is part of TypeNBT, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2018 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package net.katstuff.typenbt

import cats.parse.Parser
import net.katsstuff.typenbt.Mojangson.MojangsonParser
import net.katsstuff.typenbt._
import org.scalacheck._
import org.scalactic.Equality
import org.scalactic.anyvals.PosInt
import org.scalatest.enablers.Containing
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MojangsonTest extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks with NBTGenerator { self =>

  val successful: BeMatcher[Either[Parser.Error, _]] =
    BeMatcher[Either[Parser.Error, _]] { parsed =>
      MatchResult(
        parsed match {
          case Right(_) => true
          case Left(_)  => false
        },
        "Parser was not successful",
        "Parser was successful"
      )
    }

  implicit def parseResultContainer[A](implicit equal: Equality[A]): Containing[Either[Parser.Error, A]] =
    new Containing[Either[Parser.Error, A]] {
      override def contains(container: Either[Parser.Error, A], element: Any): Boolean =
        container match {
          case Left(_)          => false
          case Right(parsedVal) => equal.areEqual(parsedVal, element)
        }

      override def containsOneOf(container: Either[Parser.Error, A], elements: collection.Seq[Any]): Boolean =
        container match {
          case Left(_)          => false
          case Right(parsedVal) => elements.exists(equal.areEqual(parsedVal, _))
        }

      override def containsNoneOf(container: Either[Parser.Error, A], elements: collection.Seq[Any]): Boolean =
        container match {
          case Left(_)          => false
          case Right(parsedVal) => elements.forall(!equal.areEqual(parsedVal, _))
        }
    }

  private def tagNameValid(string: String): Boolean = string.nonEmpty

  test("stringLiteral should accept any string with quotes at the start and end") {
    forAll(Gen.alphaNumStr) { (string: String) =>
      val addedString = "\"" + string + "\""
      val parsed      = MojangsonParser.stringLiteral('"').parseAll(addedString)
      parsed should contain(addedString)
    }
  }
  test("stringLiteral should fail if the string is missing a quotes at the start") {
    forAll(Gen.alphaNumStr) { (string: String) =>
      val addedSign = string + "\""
      val parsed    = MojangsonParser.stringLiteral('"').parseAll(addedSign)
      parsed shouldNot contain(addedSign)
    }
  }
  test("stringLiteral should fail if the string is missing a quotes at the end") {
    forAll(Gen.alphaNumStr) { (string: String) =>
      val addedSign = "\"" + string
      val parsed    = MojangsonParser.stringLiteral('"').parseAll(addedSign)
      parsed shouldNot contain(addedSign)
    }
  }

  test("wholeNumber should accept any whole numbers, as big as longs") {
    forAll { (number: Long) =>
      val stringNumber = number.toString
      val parsed       = MojangsonParser.zNumber.parseAll(stringNumber)
      parsed should contain(number)
    }
  }

  test("floatingPoint should accept any floating point number, as big as double") {
    forAll { (number: Double) =>
      val stringNumber = number.toString
      val parsed       = MojangsonParser.floatingPoint.parseAll(stringNumber)
      parsed should contain(number)
    }
  }

  test("floatingPoint should accept any number give with dot notation") {
    forAll(Gen.choose[Double](-1, 1).suchThat(i => i != -1 && i != 1 && i != 0)) { number =>
      whenever(number.toString.charAt(0) == '0') {
        val stringNumber = number.toString.tail
        val parsed       = MojangsonParser.floatingPoint.parseAll(stringNumber)
        parsed should contain(number)
        // parsed shouldBe 'success
        // Math.abs(parsed.get.value - number) should be < 1e-7
      }
    }
  }

  test("tagName should parse nonempty strings without colon") {
    forAll(Gen.alphaNumStr) { (string: String) =>
      whenever(tagNameValid(string)) {
        val parsed = MojangsonParser.tagName.parseAll(string)
        parsed should contain(string)
      }
    }
  }

  test("nbtByte should parse a byte followed by b") {
    val string = "127b"
    val parsed = MojangsonParser.nbtByte.parseAll(string)
    parsed should contain(NBTByte(127.toByte))
  }
  test("nbtByte should fail without the b") {
    val string = "127"
    val parsed = MojangsonParser.nbtByte.parseAll(string)
    parsed shouldNot be(successful)
  }

  test("nbtShort should parse a short followed by s") {
    val string = "128s"
    val parsed = MojangsonParser.nbtShort.parseAll(string)
    parsed should contain(NBTShort(128.toShort))
  }
  test("nbtShort should fail without the s") {
    val string = "128"
    val parsed = MojangsonParser.nbtShort.parseAll(string)
    parsed shouldNot be(successful)
  }

  test("nbtLong should parse a long followed by L") {
    val string = "128L"
    val parsed = MojangsonParser.nbtLong.parseAll(string)
    parsed should contain(NBTLong(128L))
  }
  test("nbtLong should fail without the L") {
    val string = "128"
    val parsed = MojangsonParser.nbtLong.parseAll(string)
    parsed shouldNot be(successful)
  }

  test("nbtFloat should parse a float followed by F") {
    val string = "128F"
    val parsed = MojangsonParser.nbtFloat.parseAll(string)
    parsed should contain(NBTFloat(128F))
  }
  test("nbtFloat should fail without the F") {
    val string = "128"
    val parsed = MojangsonParser.nbtFloat.parseAll(string)
    parsed shouldNot be(successful)
  }

  test("nbtDouble should parse a double followed by D") {
    val string = "128D"
    val parsed = MojangsonParser.nbtDouble.parseAll(string)
    parsed should contain(NBTDouble(128D))
  }
  test("nbtDouble should fail to parse a whole number without the D") {
    val string = "128"
    val parsed = MojangsonParser.nbtDouble.parseAll(string)
    parsed shouldNot be(successful)
  }
  test("nbtDouble should parse a decimal without the D") {
    val string = "128.256"
    val parsed = MojangsonParser.nbtDouble.parseAll(string)
    parsed should contain(NBTDouble(128.256D))
  }
  test("nbtDouble should parse a decimal with the D") {
    val string = "128.256D"
    val parsed = MojangsonParser.nbtDouble.parseAll(string)
    parsed should contain(NBTDouble(128.256D))
  }

  test("nbtNumber should parse a byte followed by b") {
    val string = "127b"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTByte(127.toByte))
  }
  test("nbtNumber should parse a short followed by s") {
    val string = "128s"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTShort(128.toShort))
  }
  test("nbtNumber should parse a long followed by L") {
    val string = "128L"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTLong(128L))
  }
  test("nbtNumber should parse a float followed by F") {
    val string = "128F"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTFloat(128F))
  }
  test("nbtNumber should parse a double followed by D") {
    val string = "128D"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTDouble(128D))
  }
  test("nbtNumber should parse a decimal without the D") {
    val string = "128.256"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTDouble(128.256D))
  }
  test("nbtNumber should parse a decimal with the D") {
    val string = "128.256D"
    val parsed = MojangsonParser.nbtNumber.parseAll(string)
    parsed should contain(NBTDouble(128.256D))
  }

  test("nbtByteArray should parse arrays prefixed with B;") {
    val string = "[B;1,2,3]"
    val parsed = MojangsonParser.nbtByteArray.parseAll(string)
    parsed should contain(NBTByteArray(Vector(1, 2, 3)))
  }

  test("nbtIntArray should parse arrays prefixed with I;") {
    val string = "[I;1,2,3]"
    val parsed = MojangsonParser.nbtIntArray.parseAll(string)
    parsed should contain(NBTIntArray(Vector(1, 2, 3)))
  }

  test("nbtLongArray should parse arrays prefixed with L;") {
    val string = "[L;1,2,3]"
    val parsed = MojangsonParser.nbtLongArray.parseAll(string)
    parsed should contain(NBTLongArray(Vector(1, 2, 3)))
  }

  test("nbtArray should parse arrays prefixed with B;") {
    val string = "[B;1,2,3]"
    val parsed = MojangsonParser.nbtArray.parseAll(string)
    parsed should contain(NBTByteArray(Vector(1, 2, 3)))
  }
  test("nbtArray should parse arrays prefixed with I;") {
    val string = "[I;1,2,3]"
    val parsed = MojangsonParser.nbtArray.parseAll(string)
    parsed should contain(NBTIntArray(Vector(1, 2, 3)))
  }
  test("nbtArray should parse arrays prefixed with L;") {
    val string = "[L;1,2,3]"
    val parsed = MojangsonParser.nbtArray.parseAll(string)
    parsed should contain(NBTLongArray(Vector(1, 2, 3)))
  }

  // Last stand. Just making sure that nothing bad gets through
  test("nbtTag should parse any nbt") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration =
      self.generatorDrivenConfig.copy(minSuccessful = PosInt(200))
    forAll { (nbtTag: NBTTag) =>
      val string = Mojangson.serialize(nbtTag)
      val parsed = MojangsonParser.nbtTag.parseAll(string)
      parsed should contain(nbtTag)
    }
  }

  test("nbtNamedTag should parse a tagname, followed by a colon and then a tag") {
    forAll(genNbt, Gen.alphaNumStr) { (nbtTag: NBTTag, tagName: String) =>
      whenever(tagNameValid(tagName)) {
        val string = s"$tagName:${Mojangson.serialize(nbtTag)}"
        val parsed = MojangsonParser.nbtNamedTag.parseAll(string)
        parsed should contain((tagName, nbtTag))
      }
    }
  }

  test("indexedTag should parse a tagname, followed by a colon and then a tag") {
    forAll(genNbt, Gen.posNum[Int]) { (nbtTag, tagIndex) =>
      whenever(tagIndex >= 0) {
        val string = s"$tagIndex:${Mojangson.serialize(nbtTag)}"
        val parsed = MojangsonParser.indexedTag.parseAll(string)
        parsed should contain((Some(tagIndex), nbtTag))
      }
    }
  }

  test("nbtArrayIsh should parse arrays prefixed with B;") {
    val string = "[B;1,2,3]"
    val parsed = MojangsonParser.nbtArrayIsh.parseAll(string)
    parsed should contain(NBTByteArray(Vector(1, 2, 3)))
  }
  test("nbtArrayIsh should parse arrays prefixed with I;") {
    val string = "[I;1,2,3]"
    val parsed = MojangsonParser.nbtArrayIsh.parseAll(string)
    parsed should contain(NBTIntArray(Vector(1, 2, 3)))
  }
  test("nbtArrayIsh should parse arrays prefixed with L;") {
    val string = "[L;1,2,3]"
    val parsed = MojangsonParser.nbtArrayIsh.parseAll(string)
    parsed should contain(NBTLongArray(Vector(1, 2, 3)))
  }

  // Currently fails
  test("All parsers should ignore whitespace before a new combinator") {
    val tag    = "{ }"
    val parsed = MojangsonParser.nbtTag.parseAll(tag)
    parsed should contain(NBTCompound())
  }
}
