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

import fastparse._
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

  val successful: BeMatcher[Parsed[_]] =
    BeMatcher[Parsed[_]] { parsed =>
      MatchResult(
        parsed match {
          case Parsed.Success(_, _)    => true
          case Parsed.Failure(_, _, _) => false
        },
        "Parser was not successful",
        "Parser was successful"
      )
    }

  implicit def parseResultContainer[A: Equality]: Containing[Parsed[A]] = new Containing[Parsed[A]] {
    override def contains(container: Parsed[A], element: Any): Boolean =
      container match {
        case Parsed.Failure(_, _, _)      => false
        case Parsed.Success(parsedVal, _) => implicitly[Equality[A]].areEqual(parsedVal, element)
      }

    override def containsOneOf(container: Parsed[A], elements: collection.Seq[Any]): Boolean = {
      val equal = implicitly[Equality[A]]
      container match {
        case Parsed.Failure(_, _, _)      => false
        case Parsed.Success(parsedVal, _) => elements.exists(equal.areEqual(parsedVal, _))
      }
    }

    override def containsNoneOf(container: Parsed[A], elements: collection.Seq[Any]): Boolean = {
      val equal = implicitly[Equality[A]]
      container match {
        case Parsed.Failure(_, _, _)      => false
        case Parsed.Success(parsedVal, _) => elements.forall(!equal.areEqual(parsedVal, _))
      }
    }
  }

  private def tagNameValid(string: String): Boolean = string.nonEmpty

  test("stringLiteral should accept any string with quotes at the start and end") {
    forAll(Gen.alphaNumStr) { string: String =>
      val addedString = "\"" + string + "\""
      val parsed      = parse(addedString, MojangsonParser.stringLiteral('"')(_))
      parsed should contain(addedString)
    }
  }
  test("stringLiteral should fail if the string is missing a quotes at the start") {
    forAll(Gen.alphaNumStr) { string: String =>
      val addedSign = string + "\""
      val parsed    = parse(addedSign, MojangsonParser.stringLiteral('"')(_))
      parsed shouldNot contain(addedSign)
    }
  }
  test("stringLiteral should fail if the string is missing a quotes at the end") {
    forAll(Gen.alphaNumStr) { string: String =>
      val addedSign = "\"" + string
      val parsed    = parse(addedSign, MojangsonParser.stringLiteral('"')(_))
      parsed shouldNot contain(addedSign)
    }
  }

  test("wholeNumber should accept any whole numbers, as big as longs") {
    forAll { number: Long =>
      val stringNumber = number.toString
      val parsed       = parse(stringNumber, MojangsonParser.zNumber(_))
      parsed should contain(number)
    }
  }

  test("floatingPoint should accept any floating point number, as big as double") {
    forAll { number: Double =>
      val stringNumber = number.toString
      val parsed       = parse(stringNumber, MojangsonParser.floatingPoint(_))
      parsed should contain(number)
    }
  }

  test("floatingPoint should accept any number give with dot notation") {
    forAll(Gen.choose[Double](-1, 1).suchThat(i => i != -1 && i != 1 && i != 0)) { number =>
      whenever(number.toString.charAt(0) == '0') {
        val stringNumber = number.toString.tail
        val parsed       = parse(stringNumber, MojangsonParser.floatingPoint(_))
        parsed should contain(number)
        // parsed shouldBe 'success
        // Math.abs(parsed.get.value - number) should be < 1e-7
      }
    }
  }

  test("tagName should parse nonempty strings without colon") {
    forAll(Gen.alphaNumStr) { string: String =>
      whenever(tagNameValid(string)) {
        val parsed = parse(string, MojangsonParser.tagName(_))
        parsed should contain(string)
      }
    }
  }

  test("nbtByte should parse a byte followed by b") {
    val string = "127b"
    val parsed = parse(string, MojangsonParser.nbtByte(_))
    parsed should contain(NBTByte(127.toByte))
  }
  test("nbtByte should fail without the b") {
    val string = "127"
    val parsed = parse(string, MojangsonParser.nbtByte(_))
    parsed shouldNot be(successful)
  }

  test("nbtShort should parse a short followed by s") {
    val string = "128s"
    val parsed = parse(string, MojangsonParser.nbtShort(_))
    parsed should contain(NBTShort(128.toShort))
  }
  test("nbtShort should fail without the s") {
    val string = "128"
    val parsed = parse(string, MojangsonParser.nbtShort(_))
    parsed shouldNot be(successful)
  }

  test("nbtLong should parse a long followed by L") {
    val string = "128L"
    val parsed = parse(string, MojangsonParser.nbtLong(_))
    parsed should contain(NBTLong(128L))
  }
  test("nbtLong should fail without the L") {
    val string = "128"
    val parsed = parse(string, MojangsonParser.nbtLong(_))
    parsed shouldNot be(successful)
  }

  test("nbtFloat should parse a float followed by F") {
    val string = "128F"
    val parsed = parse(string, MojangsonParser.nbtFloat(_))
    parsed should contain(NBTFloat(128F))
  }
  test("nbtFloat should fail without the F") {
    val string = "128"
    val parsed = parse(string, MojangsonParser.nbtFloat(_))
    parsed shouldNot be(successful)
  }

  test("nbtDouble should parse a double followed by D") {
    val string = "128D"
    val parsed = parse(string, MojangsonParser.nbtDouble(_))
    parsed should contain(NBTDouble(128D))
  }

  test("nbtDouble should parse a decimal without the D") {
    val string = "128.256D"
    val parsed = parse(string, MojangsonParser.nbtDouble(_))
    parsed should contain(NBTDouble(128.256D))
  }

  test("nbtDouble should fail to parse a whole number without the D") {
    val string = "128"
    val parsed = parse(string, MojangsonParser.nbtDouble(_))
    parsed shouldNot be(successful)
  }

  test("nbtByteArray should parse arrays prefixed with B;") {
    val string = "[B;1,2,3]"
    val parsed = parse(string, MojangsonParser.nbtByteArray(_))
    parsed should contain(NBTByteArray(Vector(1, 2, 3)))
  }

  test("nbtIntArray should parse arrays prefixed with I;") {
    val string = "[I;1,2,3]"
    val parsed = parse(string, MojangsonParser.nbtIntArray(_))
    parsed should contain(NBTIntArray(Vector(1, 2, 3)))
  }

  test("nbtLongArray should parse arrays prefixed with L;") {
    val string = "[L;1,2,3]"
    val parsed = parse(string, MojangsonParser.nbtLongArray(_))
    parsed should contain(NBTLongArray(Vector(1, 2, 3)))
  }

  test("nbtNamedTag should parse a tagname, followed by a colon and then a tag") {
    forAll(genNbt, Gen.alphaNumStr) { (nbtTag: NBTTag, tagName: String) =>
      whenever(tagNameValid(tagName)) {
        val string = s"$tagName:${Mojangson.serialize(nbtTag)}"
        val parsed = parse(string, MojangsonParser.nbtNamedTag(_))
        parsed should contain((tagName, nbtTag))
      }
    }
  }

  test("indexedTag should parse a tagname, followed by a colon and then a tag") {
    forAll(genNbt, Gen.posNum[Int]) { (nbtTag, tagIndex) =>
      whenever(tagIndex >= 0) {
        val string = s"$tagIndex:${Mojangson.serialize(nbtTag)}"
        val parsed = parse(string, MojangsonParser.indexedTag(_))
        parsed should contain((Some(tagIndex), nbtTag))
      }
    }
  }

  // Last stand. Just making sure that nothing bad gets through
  test("nbtTag should parse any nbt") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration =
      self.generatorDrivenConfig.copy(minSuccessful = PosInt(200))
    forAll { nbtTag: NBTTag =>
      val string = Mojangson.serialize(nbtTag)
      val parsed = parse(string, MojangsonParser.nbtTag(_))
      parsed should contain(nbtTag)
    }
  }

  // Currently fails
  test("All parsers should ignore whitespace before a new combinator") {
    val tag    = "{ }"
    val parsed = parse(tag, MojangsonParser.nbtTag(_))
    parsed should contain(NBTCompound())
  }
}
