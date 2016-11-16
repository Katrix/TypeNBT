/*
 * This file is part of TypeNBT, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2016 Katrix
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
package net.katsstuff.typenbt.parser

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.Equality
import org.scalactic.anyvals.PosInt
import org.scalatest.enablers.Containing
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

import fastparse.noApi._
import net.katsstuff.typenbt.nbt._
import net.katsstuff.typenbt.parser.Mojangson.MojangsonParser

class MojangsonTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with NBTGenerator {
	self =>

	val successful: BeMatcher[Parsed[_]] =
		BeMatcher[Parsed[_]](left => MatchResult(left match {
			case Parsed.Success(_, _) => true
			case Parsed.Failure(_, _, _) => false
		}, "Parser was not successful", "Parser was successful"))

	implicit def parseResultContainer[A: Equality] = new Containing[Parsed[A]] {
		override def contains(container: Parsed[A], element: Any): Boolean = {
			container match {
				case Parsed.Failure(_, _, _) => false
				case Parsed.Success(parsedVal, _) => implicitly[Equality[A]].areEqual(parsedVal, element)
			}
		}
		override def containsOneOf(container: Parsed[A], elements: Seq[Any]): Boolean = {
			val eqal = implicitly[Equality[A]]
			container match {
				case Parsed.Failure(_, _, _) => false
				case Parsed.Success(parsedVal, _) => elements.exists(eqal.areEqual(parsedVal, _))
			}
		}
		override def containsNoneOf(container: Parsed[A], elements: Seq[Any]): Boolean = {
			val eqal = implicitly[Equality[A]]
			container match {
				case Parsed.Failure(_, _, _) => false
				case Parsed.Success(parsedVal, _) => elements.forall(!eqal.areEqual(parsedVal, _))
			}
		}
	}

	private def validMojangsonTag(tag: NBTTag): Boolean = tag match {
		case NBTByteArray(_) => false
		case NBTList(xs) => xs.forall(validMojangsonTag(_)) && xs.nonEmpty
		case NBTIntArray(xs) => xs.nonEmpty
		case NBTCompound(tags) => tags.forall(child => validMojangsonTag(child._2))
		case _ => true
	}

	private def stringLiteralValid(string: String): Boolean = {
		!string.contains("\"")
	}

	private def tagNameValid(string: String): Boolean = {
		string.nonEmpty && string.forall(c => !":{}[]".contains(c)) && !string.matches("""\s""")
	}

	test("stringLiteral should accept any string with quotes at the start and end") {
		forAll{ string: String =>
			whenever(stringLiteralValid(string)) {
				val addedSign = "\"" + string + "\""
				val parsed = MojangsonParser.stringLiteral.parse(addedSign)
				parsed should contain(addedSign)
			}
		}
	}
	test("stringLiteral should fail if the string is missing a quotes at the start") {
		forAll { string: String =>
			whenever(stringLiteralValid(string)) {
				val addedSign = string + "\""
				val parsed = MojangsonParser.stringLiteral.parse(addedSign)
				parsed shouldNot contain(addedSign)
			}
		}
	}
	test("stringLiteral should fail if the string is missing a quotes at the end") {
		forAll { string: String =>
			whenever(stringLiteralValid(string)) {
				val addedSign = "\"" + string
				val parsed = MojangsonParser.stringLiteral.parse(addedSign)
				parsed shouldNot contain(addedSign)
			}
		}
	}

	test("wholeNumber should accept any whole numbers, as big as longs") {
		forAll { number: Long =>
			val stringNumber = number.toString
			val parsed = MojangsonParser.wholeNumber.parse(stringNumber)
			parsed should contain(number)
		}
	}

	test("floatingPoint should accept any floating point number, as big as double") {
		forAll { number: Double =>
			val stringNumber = number.toString
			val parsed = MojangsonParser.floatingPoint.parse(stringNumber)
			parsed should contain(number)
		}
	}

	test("floatingPoint should accept any number give with dot notation") {
		forAll(Gen.choose[Double](-1, 1).suchThat(i => i != -1 && i != 1 && i != 0)) { number =>
			whenever(number.toString.charAt(0) == '0') {
				val stringNumber = number.toString.tail
				val parsed = MojangsonParser.floatingPoint.parse(stringNumber)
				parsed should contain(number)
			}
		}
	}

	test("tagName should parse nonempty strings without colon") {
		forAll { string: String =>
			whenever(tagNameValid(string)) {
				val parsed = MojangsonParser.tagName.parse(string)
				parsed should contain(string)
			}
		}
	}

	test("nbtByte should parse a byte followed by b") {
		val string = "127b"
		val parsed = MojangsonParser.nbtByte.parse(string)
		parsed should contain(NBTByte(127.toByte))
	}
	test("nbtByte should fail without the b") {
		val string = "127"
		val parsed = MojangsonParser.nbtByte.parse(string)
		parsed shouldNot be(successful)
	}

	test("nbtShort should parse a short followed by s") {
		val string = "128s"
		val parsed = MojangsonParser.nbtShort.parse(string)
		parsed should contain(NBTShort(128.toShort))
	}
	test("nbtShort should fail without the s") {
		val string = "128"
		val parsed = MojangsonParser.nbtShort.parse(string)
		parsed shouldNot be(successful)
	}

	test("nbtLong should parse a long followed by L") {
		val string = "128L"
		val parsed = MojangsonParser.nbtLong.parse(string)
		parsed should contain(NBTLong(128L))
	}
	test("nbtLong should fail without the L") {
		val string = "128"
		val parsed = MojangsonParser.nbtLong.parse(string)
		parsed shouldNot be(successful)
	}

	test("nbtFloat should parse a float followed by F") {
		val string = "128F"
		val parsed = MojangsonParser.nbtFloat.parse(string)
		parsed should contain(NBTFloat(128F))
	}
	test("nbtFloat should parse a float followed by f") {
		val string = "128f"
		val parsed = MojangsonParser.nbtFloat.parse(string)
		parsed should contain(NBTFloat(128F))
	}
	test("nbtFloat should fail without the f or F") {
		val string = "128"
		val parsed = MojangsonParser.nbtFloat.parse(string)
		parsed shouldNot be(successful)
	}

	test("nbtDouble should parse a double followed by D") {
		val string = "128D"
		val parsed = MojangsonParser.nbtDouble.parse(string)
		parsed should contain(NBTDouble(128D))
	}
	test("nbtDouble should parse a double followed by d") {
		val string = "128D"
		val parsed = MojangsonParser.nbtDouble.parse(string)
		parsed should contain(NBTDouble(128D))
	}
	test("nbtDouble should fail without the d or D") {
		val string = "128"
		val parsed = MojangsonParser.nbtDouble.parse(string)
		parsed shouldNot be(successful)
	}

	test("nbtNamedTag should parse a tagname, followed by a colon and then a tag") {
		forAll { (nbtTag: NBTTag, tagName: String) =>
			whenever(tagNameValid(tagName) && validMojangsonTag(nbtTag)) {
				val string = s"$tagName:${Mojangson.toMojangson(nbtTag)}"
				val parsed = MojangsonParser.nbtNamedTag.parse(string)
				parsed should contain((tagName, nbtTag))
			}
		}
	}

	test("indexedTag should parse a tagname, followed by a colon and then a tag") {
		forAll(genNbt, Gen.posNum[Int]) { (nbtTag, tagIndex) =>
			whenever(tagIndex >= 0 && validMojangsonTag(nbtTag)) {
				val string = s"$tagIndex:${Mojangson.toMojangson(nbtTag)}"
				val parsed = MojangsonParser.indexedTag.parse(string)
				parsed should contain((tagIndex, nbtTag))
			}
		}
	}

	//Last stand. Just making sure that nothing bad gets through
	test("nbtTag should parse any nbt") {
		implicit val generatorDrivenConfig = self.generatorDrivenConfig.copy(minSuccessful = PosInt(200))
		forAll { nbtTag: NBTTag =>
			whenever(validMojangsonTag(nbtTag)) {
				val string = Mojangson.toMojangson(nbtTag)
				val parsed = MojangsonParser.nbtTag.parse(string)
				parsed should contain(nbtTag)
			}
		}
	}

	//Currently fails
	test("All parsers should ignore whitespace before a new combinator") {
		val tag = "{ }"
		val parsed = MojangsonParser.nbtTag.parse(tag)
		parsed should contain(NBTCompound(Map()))
	}
}
