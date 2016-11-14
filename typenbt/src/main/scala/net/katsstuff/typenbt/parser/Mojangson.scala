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

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

import net.katsstuff.typenbt.nbt._

object Mojangson {

	/**
		* Parse mojangson into a [[net.katsstuff.typenbt.nbt.NBTTag]]
		*/
	def fromMojangson(mojangson: String): Either[String, NBTTag] = Parser.parse(Parser.wholeNbt, mojangson) match {
		case Parser.Success(unknownNbt, _) => Right(unknownNbt)
		case Parser.NoSuccess(msg, _) => Left(msg)
	}

	object Parser extends RegexParsers {

		type NamedTag = (String, NBTTag)
		type IndexedTag = (Int, NBTTag)
		type AnyTag = NBTView.AnyTag.NBT

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

		def nbtNumber: Parser[NBTTag] = nbtByte | nbtShort | nbtLong | nbtFloat | nbtDouble | nbtInt
		def nbtString: Parser[NBTString] = stringLiteral ^^ (s => NBTString(s.substring(1, s.length - 1)))

		def nbtTag: Parser[NBTTag] = nbtNumber | nbtString | nbtCompound | nbtList | nbtIntArray

		def nbtNamedTag: Parser[NamedTag] = tagName ~ colon ~ nbtTag ^^ { case name ~ _ ~ tag => (name, tag) }
		def nbtCompound: Parser[NBTCompound] = compoundStart ~> repsep(nbtNamedTag, comma) <~ compoundEnd ^^ (xs => NBTCompound(xs.toMap))
		def nbtIntArray: Parser[NBTIntArray] = listStart ~> repsep(wholeNumber, comma) <~ listEnd ^^ {xs => NBTIntArray(xs.map(_.toInt).toVector)}

		def indexedTag: Parser[IndexedTag] = tagIndex ~ colon ~ nbtTag ^^ { case index ~ _ ~ tag => index -> tag }
		def nbtList: Parser[NBTList[Any, NBTTag.Aux[Any]]] = listStart ~> repsep(indexedTag, comma) <~ listEnd ^? {
			case list@((_, head)) :: tail if list.forall(a => a._2.nbtType.id == head.nbtType.id) && {
				@tailrec
				def checkIndex(rest: List[(Int, _)], i: Int): Boolean = rest match {
					case Nil => true
					case x :: xs if x._1 == i => checkIndex(xs, i + 1)
					case _ => false
				}

				checkIndex(list, 0)
			} => NBTList[Any, AnyTag](list.map(_._2).asInstanceOf[List[AnyTag]])(NBTView.TAG_LIST,
				list.head._2.nbtType.asInstanceOf[NBTType.Aux[Any, AnyTag]])
			case Nil => NBTList[Byte, NBTByte](Seq()).asInstanceOf[NBTList[Any, AnyTag]] //We use byte if there are no elements
		}

		def wholeNbt: Parser[NBTCompound] = phrase(nbtCompound)
	}

	/**
		* Convert a [[net.katsstuff.typenbt.nbt.NBTTag]] to mojangson.
		*/
	def toMojangson(tag: NBTTag): String = tag match {
		case NBTByte(b) => s"${b}b"
		case NBTShort(s) => s"${s}s"
		case NBTInt(i) => s"$i"
		case NBTLong(l) => s"${l}L"
		case NBTFloat(f) => s"${f}f"
		case NBTDouble(d) => s"${d}d"
		case NBTByteArray(array) => s"[${array.length} bytes]"
		case NBTString(s) => s""""$s""""
		case NBTList(list) =>
			val b = new StringBuilder("[")

			for((tag, index) <- list.zipWithIndex) {
				if(index != 0) {
					b.append(',')
				}
				b.append(s"$index:${toMojangson(tag)}")
			}

			b.append(']').mkString
		case NBTCompound(tags) =>
			val b = new StringBuilder("{")

			for(((name, tag)) <- tags) {
				if(b.length != 1) {
					b.append(',')
				}
				b.append(s"$name:${toMojangson(tag)}")
			}

			b.append('}').mkString
		case NBTIntArray(array) =>
			val b = new StringBuilder("[")

			for(i <- array) {
				if(b.length != 1) {
					b.append(',')
				}
				b.append(s"$i")
			}

			b.append(']').mkString
	}

	/**
		* Convert a [[net.katsstuff.typenbt.nbt.NBTTag]] to mojangson with indentation
		* @param tag The tag to convert
		* @param indentLevel How many indent characters to insert per level
		* @param indentChar The indent character to use
		*/
	def toMojangsonIndent(tag: NBTTag, indentLevel: Int = 1, indentChar: Char = '	'): String = {
		def indent(b: StringBuilder, indentLevel: Int): Unit = {
			b.append('\n')
			(0 to indentLevel).foreach(_ => b.append(indentChar))
		}

		tag match {
			case NBTList(list) =>
				val b = new StringBuilder("[")

				for((tag, index) <- list.zipWithIndex) {
					if(index != 0) {
						b.append(',')
					}
					indent(b, indentLevel)
					b.append(s"$index:${toMojangsonIndent(tag, indentLevel + 1, indentChar)}")
				}

				if(list.nonEmpty) {
					indent(b, indentLevel - 1)
				}
				b.append(']').mkString

			case NBTCompound(tags) =>
				val b = new StringBuilder("{")

				for(((name, tag)) <- tags) {
					if(b.length != 1) {
						b.append(',')
					}

					indent(b, indentLevel)
					b.append(s"$name:${toMojangsonIndent(tag, indentLevel + 1, indentChar)}")
				}

				if(tags.nonEmpty) {
					indent(b, indentLevel - 1)
				}

				b.append('}').mkString
			case _ => toMojangson(tag)
		}
	}
}