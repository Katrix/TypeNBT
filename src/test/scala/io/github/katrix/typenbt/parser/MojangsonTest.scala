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
import org.scalatest.{FlatSpec, Matchers}

import io.github.katrix.typenbt.misc.AST.{NBTByte, NBTCompound, NBTDouble, NBTFloat, NBTInt, NBTList, NBTLong, NBTShort, NBTString, NamedTag}
import io.github.katrix.typenbt.parser.Mojangson.Parser

class MojangsonTest extends FlatSpec with Matchers {

	val successful = BeMatcher[Parser.ParseResult[_]](left => MatchResult(left.successful, "Parser was not successful", "Parser was successful"))
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

	"A string literal" should "accept any string" in {
		val testedString = "\"abcd5 r2564\""
		val parsed = Parser.parseAll(Parser.stringLiteral, testedString)
		parsed should contain (testedString)
	}
	it should "not be valid without quotes at the start and end" in {
		val testedString = "without"
		val parsed = Parser.parseAll(Parser.stringLiteral, testedString)
		parsed shouldNot be (successful)
	}
	it should "allow escaping quotes" in {
		val testedString = """"\"""""
		val parsed = Parser.parseAll(Parser.stringLiteral, testedString)
		parsed should contain (testedString)
	}

	"A whole number" should "accept whole numbers" in {
		val number = 12345
		val stringNumber = number.toString
		val parsed = Parser.parseAll(Parser.wholeNumber, stringNumber)
		parsed should contain (number)
	}
	it should "allow negative numbers" in {
		val number = -100
		val stringNumber = number.toString
		val parsed = Parser.parseAll(Parser.wholeNumber, stringNumber)
		parsed should contain (number)
	}
	it should "parse longs" in {
		val number = Long.MaxValue
		val stringNumber = number.toString
		val parsed = Parser.parse(Parser.wholeNumber, stringNumber)
		parsed should contain (number)
	}
	it should "fail on anything other than numbers" in {
		val string = "Hi"
		val parsed = Parser.parseAll(Parser.wholeNumber, string)
		parsed shouldNot be (successful)
	}

	"A floating point number" should "accept floating point numbers" in {
		val number = 23F
		val stringNumber = number.toString
		val parsed = Parser.parseAll(Parser.floatingPoint, stringNumber)
		parsed should contain (number)
	}
	it should "allow negative numbers" in {
		val number = -100F
		val stringNumber = number.toString
		val parsed = Parser.parse(Parser.floatingPoint, stringNumber)
		parsed should contain (number)
	}
	it should "allow scientific notation" in {
		val stringNumber = "2.5E-1"
		val parsed = Parser.parseAll(Parser.floatingPoint, stringNumber)
		parsed should contain (0.25D)
	}
	it should "parse doubles" in {
		val number = Double.MaxValue
		val stringNumber = number.toString
		val parsed = Parser.parseAll(Parser.floatingPoint, stringNumber)
		parsed should contain (number)
	}
	it should "parse numbers with . as first char" in {
		val stringNumber = ".25"
		val parsed = Parser.parseAll(Parser.floatingPoint, stringNumber)
		parsed should contain (0.25D)
	}
	it should "fail on anything other than numbers" in {
		val string = "Hi"
		val parsed = Parser.parseAll(Parser.floatingPoint, string)
		parsed shouldNot be (successful)
	}

	"Colon" should "parse colons" in {
		val string = ":"
		val parsed = Parser.parseAll(Parser.colon, string)
		parsed should contain (':')
	}
	it should "fail on anything else" in {
		val string = "Hi"
		val parsed = Parser.parseAll(Parser.colon, string)
		parsed shouldNot be (successful)
	}

	"Comma" should "parse commas" in {
		val string = ","
		val parsed = Parser.parseAll(Parser.comma, string)
		parsed should contain (',')
	}
	it should "fail on anything else" in {
		val string = "Something else here"
		val parsed = Parser.parseAll(Parser.comma, string)
		parsed shouldNot be (successful)
	}

	"TagName" should "parse strings" in {
		val string = "some text here"
		val parsed = Parser.parseAll(Parser.tagName, string)
		parsed should contain (string)
	}
	it should "fail on string with colons" in {
		val string = "this:"
		val parsed = Parser.parseAll(Parser.tagName, string)
		parsed shouldNot be (successful)
	}

	"TagIndex" should "parse numbers" in {
		val index = 1
		val stringNumber = index.toString
		val parsed = Parser.parseAll(Parser.tagIndex, stringNumber)
		parsed should contain (index)
	}
	it should "fail on negative numbers" in {
		val index = -1
		val stringNumber = index.toString
		val parsed = Parser.parseAll(Parser.tagIndex, stringNumber)
		parsed shouldNot be (successful)
	}

	"CompoundStart" should "parse {" in {
		val string = "{"
		val parsed = Parser.parseAll(Parser.compoundStart, string)
		parsed should contain ('{')
	}
	it should "fail on anything else" in {
		val string = "something else"
		val parsed = Parser.parseAll(Parser.compoundStart, string)
		parsed shouldNot be (successful)
	}

	"CompoundEnd" should "parse }" in {
		val string = "}"
		val parsed = Parser.parseAll(Parser.compoundEnd, string)
		parsed should contain ('}')
	}
	it should "fail on anything else" in {
		val string = "whatever"
		val parsed = Parser.parseAll(Parser.compoundEnd, string)
		parsed shouldNot be (successful)
	}

	"ListStart" should "parse [" in {
		val string = "["
		val parsed = Parser.parseAll(Parser.listStart, string)
		parsed should contain ('[')
	}
	it should "fail on anything else" in {
		val string = "running out of ideas here"
		val parsed = Parser.parseAll(Parser.listStart, string)
		parsed shouldNot be (successful)
	}

	"ListEnd" should "parse ]" in {
		val string = "]"
		val parsed = Parser.parseAll(Parser.listEnd, string)
		parsed should contain (']')
	}
	it should "fail on anything else" in {
		val string = "help me"
		val parsed = Parser.parseAll(Parser.listEnd, string)
		parsed shouldNot be (successful)
	}

	"ByteEnd" should "parse b" in {
		val string = "b"
		val parsed = Parser.parseAll(Parser.byteEnd, string)
		parsed should contain ('b')
	}
	it should "fail on anything else" in {
		val string = "soon done now"
		val parsed = Parser.parseAll(Parser.byteEnd, string)
		parsed shouldNot be (successful)
	}

	"ShortEnd" should "parse s" in {
		val string = "s"
		val parsed = Parser.parseAll(Parser.shortEnd, string)
		parsed should contain ('s')
	}
	it should "fail on anything else" in {
		val string = "just a few more"
		val parsed = Parser.parseAll(Parser.shortEnd, string)
		parsed shouldNot be (successful)
	}

	"LongEnd" should "parse L" in {
		val string = "L"
		val parsed = Parser.parseAll(Parser.longEnd, string)
		parsed should contain ('L')
	}
	it should "fail on anything else" in {
		val string = "last single?"
		val parsed = Parser.parseAll(Parser.longEnd, string)
		parsed shouldNot be (successful)
	}

	"FloatEnd" should "parse f" in {
		val string = "f"
		val parsed = Parser.parseAll(Parser.floatEnd, string)
		parsed should contain ('f')
	}
	it should "parse F" in {
		val string = "F"
		val parsed = Parser.parseAll(Parser.floatEnd, string)
		parsed should contain ('F')
	}
	it should "fail on anything else" in {
		val string = "I can see the light"
		val parsed = Parser.parseAll(Parser.floatEnd, string)
		parsed shouldNot be (successful)
	}

	"DoubleEnd" should "parse d" in {
		val string = "d"
		val parsed = Parser.parseAll(Parser.doubleEnd, string)
		parsed should contain ('d')
	}
	it should "parse D" in {
		val string = "D"
		val parsed = Parser.parseAll(Parser.doubleEnd, string)
		parsed should contain ('D')
	}
	it should "fail on anything else" in {
		val string = "And I'm out"
		val parsed = Parser.parseAll(Parser.doubleEnd, string)
		parsed shouldNot be (successful)
	}

	"NBTByte" should "parse a byte followed by byteEnd" in {
		val string = "128b"
		val parsed = Parser.parseAll(Parser.nbtByte, string)
		parsed should contain (NBTByte(128.toByte))
	}
	it should "fail without the b" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtByte, string)
		parsed shouldNot be (successful)
	}

	"NBTShort" should "parse a short followed by shortEnd" in {
		val string = "128s"
		val parsed = Parser.parseAll(Parser.nbtShort, string)
		parsed should contain (NBTShort(128.toShort))
	}
	it should "fail without the s" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtShort, string)
		parsed shouldNot be (successful)
	}

	"NBTLong" should "parse a long followed by longEnd" in {
		val string = "128L"
		val parsed = Parser.parseAll(Parser.nbtLong, string)
		parsed should contain (NBTLong(128L))
	}
	it should "fail without the L" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtLong, string)
		parsed shouldNot be (successful)
	}

	"NBTFloat" should "parse a float followed by floatEnd" in {
		val string = "128F"
		val parsed = Parser.parseAll(Parser.nbtFloat, string)
		parsed should contain (NBTFloat(128F))
	}
	it should "fail without the f or F" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtFloat, string)
		parsed shouldNot be (successful)
	}

	"NBTDouble" should "parse a double followed by doubleEnd" in {
		val string = "128D"
		val parsed = Parser.parseAll(Parser.nbtDouble, string)
		parsed should contain (NBTDouble(128D))
	}
	it should "fail without the d or D" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtDouble, string)
		parsed shouldNot be (successful)
	}

	"NBTInt" should "parse an int" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtInt, string)
		parsed should contain (NBTInt(128))
	}
	it should "fail if other chars follow it" in {
		val string = "128D"
		val parsed = Parser.parseAll(Parser.nbtInt, string)
		parsed shouldNot be (successful)
	}

	"NBTNumber" should "parse a NBTByte" in {
		val string = "128b"
		val parsed = Parser.parseAll(Parser.nbtByte, string)
		parsed should contain (NBTByte(128.toByte))
	}
	it should "parse a NBTShort in" in {
		val string = "128s"
		val parsed = Parser.parseAll(Parser.nbtShort, string)
		parsed should contain (NBTShort(128.toShort))
	}
	it should "parse a NBTLong" in {
		val string = "128L"
		val parsed = Parser.parseAll(Parser.nbtLong, string)
		parsed should contain (NBTLong(128L))
	}
	it should "parse a NBTFloat" in {
		val string = "128F"
		val parsed = Parser.parseAll(Parser.nbtFloat, string)
		parsed should contain (NBTFloat(128F))
	}
	it should "parse a NBTDouble" in {
		val string = "128D"
		val parsed = Parser.parseAll(Parser.nbtDouble, string)
		parsed should contain (NBTDouble(128D))
	}
	it should "parse a NBTInt" in {
		val string = "128"
		val parsed = Parser.parseAll(Parser.nbtInt, string)
		parsed should contain (NBTInt(128))
	}

	"NBTString" should "parse a NBTString from a string literal" in {
		val content = "Some content here"
		val string = s""""$content""""
		val parsed = Parser.parseAll(Parser.nbtString, string)
		parsed should contain (NBTString(content))
	}

	"NBTNamedValue" should "parse a tagname, followed by a colon and then a tag" in {
		val tagName = "name"
		val content = "some content"
		val string = s""""$content""""
		val tag = s"$tagName:$string"
		val parsed = Parser.parseAll(Parser.nbtNamedTag, tag)
		parsed should contain (NamedTag((tagName, NBTString(content))))
	}

	"NBTIndexedTag" should "parse an index, followed by a colon and then a tag" in {
		val tagIndex = 1
		val content = "some content"
		val string = s""""$content""""
		val tag = s"$tagIndex:$string"
		val parsed = Parser.parseAll(Parser.indexedTag, tag)
		parsed should contain ((tagIndex, NBTString(content)))
	}

	"NBTList" should "parse an listStart followed by an arbitrary number of nbtIndexedTag separated by commas, ending with a listEnd" in {
		val tag = "[0:1,1:2,2:3]"
		val parsed = Parser.parseAll(Parser.nbtList, tag)
		parsed should contain (NBTList(3 /*ID for ints*/, Seq(NBTInt(1), NBTInt(2), NBTInt(3))))
	}

	"NBTCompound" should "parse an compoundStart followed an arbitrary number of nbtNamedTag, ending with a compoundEnd" in {
		val tag = "{first:1,second:2,third:3}"
		val parsed = Parser.parseAll(Parser.nbtCompound, tag)
		parsed should contain (NBTCompound(Seq(NamedTag(("first", NBTInt(1))), NamedTag(("second", NBTInt(2))), NamedTag(("third", NBTInt(3))))))
	}

	"NBTTag" should "parse all types of tags" in {
		val tag = """[0:{int:5},1:{string:"someString"},2:{}]"""
		val first = NBTCompound(Seq(NamedTag(("int", NBTInt(5)))))
		val second = NBTCompound(Seq(NamedTag(("string", NBTString("someString")))))
		val third = NBTCompound(Seq())
		val expected = NBTList(10 /*ID for compounds*/, Seq(first, second, third))
		val parsed = Parser.parseAll(Parser.nbtTag, tag)
		parsed should contain (expected)
	}

	//Currently fails
	"All parsers" should "ignore whitespace before a new combinator" ignore {
		val tag = "{ }"
		val parsed = Parser.parseAll(Parser.nbtTag, tag)
		parsed should contain (NBTCompound(Seq()))
	}
}
