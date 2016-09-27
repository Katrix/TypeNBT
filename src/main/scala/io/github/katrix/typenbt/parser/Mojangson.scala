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

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

import io.github.katrix.typenbt.misc.AST
import io.github.katrix.typenbt.misc.AST._
import io.github.katrix.typenbt.nbt

object Mojangson {

	def mojangsonToAST(mojangson: String): Parser.ParseResult[NBTCompound] = Parser.parse(Parser.wholeNbt, mojangson)
	def mojangsonToNBT(mojangson: String): Either[String, nbt.NBTTag[_]] = mojangsonToAST(mojangson) match {
		case Parser.Success(AST(unknownNbt), _) => Right(unknownNbt)
		case Parser.Error(msg, _) => Left(msg)
		case Parser.Failure(msg, _) => Left(msg)
	}

	def nbtToMojangson(tag: nbt.NBTTag[_]): String = toMojangson(AST(tag))

	object Parser extends RegexParsers {

		def stringLiteral: Parser[String] = ("\"" + """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+""" + "\"").r
		def wholeNumber: Parser[Long] = """-?\d+""".r ^^ {_.toLong}
		def floatingPoint: Parser[Double] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ {_.toDouble}

		def colon: Parser[Char] = ':'
		def comma: Parser[Char] = ','
		def tagName: Parser[String] = """^([^:]+)""".r
		def tagIndex: Parser[Int] = """\d+""".r ^^ (_.toInt)

		def compoundStart: Parser[Char] = '{'
		def compoundEnd: Parser[Char] = '}'
		def listStart: Parser[Char] = '['
		def listEnd: Parser[Char] = ']'

		def byteEnd: Parser[Char] = 'b'
		def shortEnd: Parser[Char] = 's'
		def longEnd: Parser[Char] = 'L'
		def floatEnd: Parser[Char] = elem('f') | 'F'
		def doubleEnd: Parser[Char] = elem('d') | 'D'

		def nbtByte: Parser[NBTByte] = wholeNumber <~ byteEnd ^^ (numb => NBTByte(numb.toByte))
		def nbtShort: Parser[NBTShort] = wholeNumber <~ shortEnd ^^ (numb => NBTShort(numb.toShort))
		def nbtLong: Parser[NBTLong] = wholeNumber <~ longEnd ^^ (numb => NBTLong(numb))
		def nbtFloat: Parser[NBTFloat] = floatingPoint <~ floatEnd ^^ (numb => NBTFloat(numb.toFloat))
		def nbtDouble: Parser[NBTDouble] = floatingPoint <~ doubleEnd ^^ (numb => NBTDouble(numb))
		def nbtInt: Parser[NBTInt] = wholeNumber ^^ (numb => NBTInt(numb.toInt))

		def nbtNumber: Parser[Tag[_]] = nbtByte | nbtShort | nbtLong | nbtFloat | nbtDouble | nbtInt
		def nbtString: Parser[NBTString] = stringLiteral ^^ (s => NBTString(s.substring(1, s.length - 1)))

		def nbtTag: Parser[Tag[_]] = nbtNumber | nbtString | nbtCompound | nbtList

		def nbtNamedTag: Parser[NamedTag] = tagName ~ colon ~ nbtTag ^^ { case name ~ _ ~ tag => NamedTag(name, tag) }
		def nbtCompound: Parser[NBTCompound] = compoundStart ~> repsep(nbtNamedTag, comma) <~ compoundEnd ^^ (NBTCompound(_))

		def indexedTag: Parser[(Int, Tag[_])] = tagIndex ~ colon ~ nbtTag ^^ { case index ~ _ ~ tag => index -> tag }
		def nbtList: Parser[NBTList] = listStart ~> repsep(indexedTag, comma) <~ listEnd ^? {
			case list@((_, head)) :: tail if list.forall(a => a._2.id == head.id) && {
				@tailrec
				def checkIndex(rest: List[(Int, _)], i: Int): Boolean = rest match {
					case Nil => true
					case x :: xs if x._1 == i => checkIndex(xs, i + 1)
					case _ => false
				}

				checkIndex(list, 0)
			} => NBTList(head.id, list.map(_._2))
		}

		def wholeNbt: Parser[NBTCompound] = phrase(nbtCompound)
	}

	def toMojangson(tag: Tag[_]): String = tag match {
		case NBTByte(b) => s"${b}b"
		case NBTShort(s) => s"${s}s"
		case NBTInt(i) => s"$i"
		case NBTLong(l) => s"${l}L"
		case NBTFloat(f) => s"${f}f"
		case NBTDouble(d) => s"${d}d"
		case NBTByteArray(array) => s"[${array.length} bytes]"
		case NBTString(s) => s""""$s""""
		case NBTList(_, list) =>

			@tailrec
			def inner(index: Int, rest: Seq[Tag[_]], acc: StringBuilder): StringBuilder = {
				if(rest == Nil) acc
				else {
					if(index != 0) acc.append(',')
					inner(index + 1, rest.tail, acc.append(s"$index:${toMojangson(rest.head)}"))
				}
			}

			inner(0, list, new StringBuilder("[")).append(']').mkString
		case NBTCompound(tags) =>
			val b = new StringBuilder("{")

			for(NamedTag((name, tag)) <- tags.seq) {
				if(b.length != 1) {
					b.append(',')
				}
				b.append(s"$name:${toMojangson(tag)}")
			}

			b.append('}').toString

		case NBTIntArray(array) =>
			val b = new StringBuilder("[")
			array.foreach(i => b.append(s"$i,"))
			b.append(']').mkString
	}

	def toMojangsonIndent(tag: Tag[_], indentLevel: Int): String = {
		def indent(b: StringBuilder, indentLevel: Int): StringBuilder = {
			b.append('\n')
			(0 to indentLevel).foreach(i => b.append('\t'))
			b
		}

		tag match {
			case NBTList(_, list) =>
				@tailrec
				def inner(index: Int, rest: Seq[Tag[_]], acc: StringBuilder): StringBuilder = {
					if(rest == Nil) acc
					else {
						if(index != 0) acc.append(',')
						indent(acc, indentLevel)
						inner(index + 1, rest.tail, acc.append(s"$index:${toMojangson(rest.head)}"))
					}
				}

				val b = inner(0, list, new StringBuilder("["))
				indent(b, indentLevel - 1)
				b.append(']').toString

			case NBTCompound(tags) =>
				val b = new StringBuilder("{")

				for(NamedTag((name, tag)) <- tags.seq) {
					if(b.length != 1) {
						b.append(',')
					}

					indent(b, indentLevel)
					b.append(s"$name:${toMojangsonIndent(tag, indentLevel + 1)}")
				}

				if(tags.nonEmpty) {
					indent(b, indentLevel - 1)
				}

				b.append('}').toString
			case _ => toMojangson(tag)
		}
	}
}