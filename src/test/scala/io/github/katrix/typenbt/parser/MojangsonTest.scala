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
package io.github.katrix.typenbt.parser

import org.scalactic.Equality
import org.scalatest.enablers.Containing
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

import io.github.katrix.typenbt.nbt._
import io.github.katrix.typenbt.parser.Mojangson.Parser
import org.scalacheck._
import org.scalactic.anyvals.PosInt

class MojangsonTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with NBTGenerator { self =>

	val successful: BeMatcher[Parser.ParseResult[_]] =
		BeMatcher[Parser.ParseResult[_]](left => MatchResult(left.successful, "Parser was not successful", "Parser was successful"))

	implicit def parseResultContainer[A : Equality] = new Containing[Parser.ParseResult[A]] {
		override def contains(container: Parser.ParseResult[A], element: Any): Boolean = container.successful && implicitly[Equality[A]].areEqual(container.get, element)
		override def containsOneOf(container: Parser.ParseResult[A], elements: Seq[Any]): Boolean = {
			val eqal = implicitly[Equality[A]]
			if(container.successful) {
				val gotten = container.get
				elements.exists(eqal.areEqual(gotten, _))
			} else false
		}
		override def containsNoneOf(container: Parser.ParseResult[A], elements: Seq[Any]): Boolean = {
			val eqal = implicitly[Equality[A]]
			if(container.successful) {
				val gotten = container.get
				elements.forall(!eqal.areEqual(gotten, _))
			} else true
		}
	}

	def isInvalidMojangsonTag(tag: NBTTag): Boolean = tag match {
		case NBTByteArray(_) => true
		case NBTList(xs) => xs.exists(isInvalidMojangsonTag(_))
		case NBTIntArray(xs) => xs.isEmpty
		case NBTCompound(tags)	=> tags.exists(child => isInvalidMojangsonTag(child._2))
		case _ => false
	}

	test("stringLiteral should accept any string with qoutes at the start and end") {
		forAll { string: String =>
			val addedSign = "\"" + string + "\""
			val parsed = Parser.parseAll(Parser.stringLiteral, addedSign)
			parsed should contain (addedSign)
		}
	}
	test("stringLiteral should fail if the string is missing a qoute at the start") {
		forAll { string: String =>
			val addedSign = string + "\""
			val parsed = Parser.parseAll(Parser.stringLiteral, addedSign)
			parsed shouldNot contain (addedSign)
		}
	}
	test("stringLiteral should fail if the string is missing a qoute at the end") {
		forAll { string: String =>
			val addedSign = "\"" + string
			val parsed = Parser.parseAll(Parser.stringLiteral, addedSign)
			parsed shouldNot contain (addedSign)
		}
	}

	test("wholeNumber should accept any whole numbers, as big as longs") {
		forAll { number: Long =>
			val stringNumber = number.toString
			val parsed = Parser.parseAll(Parser.wholeNumber, stringNumber)
			parsed should contain (number)
		}
	}

	test("floatingPoint should accept any floating point number, as big as double") {
		forAll { number: Double =>
			val stringNumber = number.toString
			val parsed = Parser.parseAll(Parser.floatingPoint, stringNumber)
			parsed should contain (number)
		}
	}

	test("floatingPoint should accept any number give with dot notation") {
		forAll(Gen.chooseNum[Double](-1, 1).suchThat(i => i != -1 && i != 1)) { number =>
			whenever(number.toString.charAt(0) == '0') {
				val stringNumber = number.toString.tail
				val parsed = Parser.parseAll(Parser.floatingPoint, stringNumber)
				parsed should contain (number)
			}
		}
	}

	test("tagName should parse strings nonempty without colon") {
		forAll { string: String =>
			whenever(!string.contains(':') && string.nonEmpty) {
				val parsed = Parser.parseAll(Parser.tagName, string)
				parsed should contain (string)
			}
		}
	}

	test("nbtByte should parse a byte followed by b") {
		val string = "128b"
		val parsed = Parser.parseAll(Parser.nbtByte, string)
		parsed should contain (NBTByte(128.toByte))
	}
	test("nbtByte should fail without the b") {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtByte, string)
		parsed shouldNot be (successful)
	}

	test("nbtShort should parse a short followed by s") {
		val string = "128s"
		val parsed = Parser.parseAll(Parser.nbtShort, string)
		parsed should contain (NBTShort(128.toShort))
	}
	test("nbtShort should fail without the s") {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtShort, string)
		parsed shouldNot be (successful)
	}

	test("nbtLong should parse a long followed by L") {
		val string = "128L"
		val parsed = Parser.parseAll(Parser.nbtLong, string)
		parsed should contain (NBTLong(128L))
	}
	test("nbtLong should fail without the L") {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtLong, string)
		parsed shouldNot be (successful)
	}

	test("nbtFloat should parse a float followed by F") {
		val string = "128F"
		val parsed = Parser.parseAll(Parser.nbtFloat, string)
		parsed should contain (NBTFloat(128F))
	}
	test("nbtFloat should parse a float followed by f") {
		val string = "128f"
		val parsed = Parser.parseAll(Parser.nbtFloat, string)
		parsed should contain (NBTFloat(128F))
	}
	test("nbtFloat should fail without the f or F") {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtFloat, string)
		parsed shouldNot be (successful)
	}

	test("nbtDouble should parse a double followed by D") {
		val string = "128D"
		val parsed = Parser.parseAll(Parser.nbtDouble, string)
		parsed should contain (NBTDouble(128D))
	}
	test("nbtDouble should parse a double followed by d") {
		val string = "128D"
		val parsed = Parser.parseAll(Parser.nbtDouble, string)
		parsed should contain (NBTDouble(128D))
	}
	test("nbtDouble should fail without the d or D") {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtDouble, string)
		parsed shouldNot be (successful)
	}

	test("nbtNamedTag should parse a tagname, followed by a colon and then a tag") {
		forAll {(nbtTag: NBTTag, tagName: String) =>
			whenever(!tagName.contains(':') && tagName.nonEmpty && !isInvalidMojangsonTag(nbtTag)) {
				val string = s"$tagName:${Mojangson.nbtToMojangson(nbtTag)}"
				val parsed = Parser.parseAll(Parser.nbtNamedTag, string)
				parsed should contain ((tagName, nbtTag))
			}
		}
	}

	test("indexedTag should parse a tagname, followed by a colon and then a tag") {
		forAll {(nbtTag: NBTTag, tagIndex: Int) =>
			whenever(tagIndex >= 0 && !isInvalidMojangsonTag(nbtTag)) {
				val string = s"$tagIndex:${Mojangson.nbtToMojangson(nbtTag)}"
				val parsed = Parser.parseAll(Parser.indexedTag, string)
				parsed should contain ((tagIndex, nbtTag))
			}
		}
	}

	//Last stand. Just making sure that nothing bad gets through
	test("nbtTag should parse any nbt") {
		implicit val generatorDrivenConfig  = self.generatorDrivenConfig.copy(minSuccessful = PosInt(200))
		forAll {nbtTag: NBTTag =>
			whenever(!isInvalidMojangsonTag(nbtTag)) {
				val string = Mojangson.nbtToMojangson(nbtTag)
				val parsed = Parser.parseAll(Parser.nbtTag, string)
				parsed should contain (nbtTag)
			}
		}
	}

	//Currently fails
	test("All parsers should ignore whitespace before a new combinator") {
		val tag = "{ }"
		val parsed = Parser.parseAll(Parser.nbtTag, tag)
		parsed should contain (NBTCompound(Map()))
	}
}
