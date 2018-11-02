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
package net.katsstuff.typenbt

import fastparse._
import fastparse.MultiLineWhitespace._

object Mojangson {

  /**
		* Parse mojangson into a [[net.katsstuff.typenbt.NBTTag]]
		*/
  def deserialize(mojangson: String): Parsed[NBTCompound] = parse(mojangson, MojangsonParser.wholeNbt(_))

  object MojangsonParser {

    type NamedTag   = (String, NBTTag)
    type IndexedTag = (Int, NBTTag)

    def nNumber[_: P]: P[Unit] = P(CharsWhileIn("0-9"))

    // Represents the regex \"(\\.|[^\\"])*\"
    def stringLiteral[_: P]: P[String] =
      P(
        "\"" ~ (("\\" ~ AnyChar) | CharPred(c => c != '\\' && c != '"')).rep ~ "\""
      ).!.map(_.replace("\\\"", "\"").replace("\\\\", "\\")).opaque("String literal")

    // Represents the regex [-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?
    def floatingPoint[_: P]: P[Double] =
      P(
        CharIn("+\\-").? ~ (&(CharsWhileIn("0-9") ~ ".") ~ CharsWhileIn("0-9")).? ~ ".".? ~ nNumber ~ (CharIn("eE") ~ CharIn(
          "+\\-"
        ).? ~ nNumber).?
      ).!.map(_.toDouble) //.opaque("Floating point number")

    def zNumber[_: P]: P[Long] = P("-".? ~ nNumber).!.map(_.toLong).opaque("Whole number")

    def colon[_: P]: P[Unit]     = P(":")
    def comma[_: P]: P[Unit]     = P(",")
    def tagName[_: P]: P[String] = P(CharsWhile(c => !":{}[]".contains(c))).!.opaque("Tag name") //Better way?
    def tagIndex[_: P]: P[Int]   = P(nNumber).!.map(_.toInt).opaque("Tag index")

    def compoundStart[_: P]: P[Unit] = P("{").opaque("Compound start")
    def compoundEnd[_: P]: P[Unit]   = P("}").opaque("Compound end")
    def listStart[_: P]: P[Unit]     = P("[").opaque("List start")
    def listEnd[_: P]: P[Unit]       = P("]").opaque("List end")

    def byteEnd[_: P]: P[Unit]   = P("b").opaque("Byte end")
    def shortEnd[_: P]: P[Unit]  = P("s").opaque("Short end")
    def longEnd[_: P]: P[Unit]   = P("L").opaque("Long end")
    def floatEnd[_: P]: P[Unit]  = P(CharIn("fF")).opaque("Float end")
    def doubleEnd[_: P]: P[Unit] = P(CharIn("dD")).opaque("Double end")

    def nbtByte[_: P]: P[NBTByte]     = P(zNumber ~ byteEnd).map(n => NBTByte(n.toByte))
    def nbtShort[_: P]: P[NBTShort]   = P(zNumber ~ shortEnd).map(n => NBTShort(n.toShort))
    def nbtLong[_: P]: P[NBTLong]     = P(zNumber ~ longEnd).map(n => NBTLong(n))
    def nbtFloat[_: P]: P[NBTFloat]   = P(floatingPoint ~ floatEnd).map(n => NBTFloat(n.toFloat))
    def nbtDouble[_: P]: P[NBTDouble] = P(floatingPoint ~ doubleEnd).map(n => NBTDouble(n))
    def nbtInt[_: P]: P[NBTInt]       = P(zNumber.map(n => NBTInt(n.toInt)))

    def nbtNumber[_: P]: P[NBTTag]    = P(nbtByte | nbtShort | nbtLong | nbtFloat | nbtDouble | nbtInt)
    def nbtString[_: P]: P[NBTString] = P(stringLiteral.map(s => NBTString(s.substring(1, s.length - 1))))

    def nbtTag[_: P]: P[NBTTag] = P(nbtNumber | nbtString | nbtCompound | NoCut(nbtList) | nbtIntArray)

    def nbtNamedTag[_: P]: P[NamedTag] = P(tagName ~/ colon ~/ nbtTag)
    def nbtCompound[_: P]: P[NBTCompound] =
      P(compoundStart ~/ nbtNamedTag.rep(sep = comma./) ~/ compoundEnd).map(xs => NBTCompound(xs.toMap))
    def nbtIntArray[_: P]: P[NBTIntArray] =
      P(listStart ~/ zNumber.rep(sep = comma./) ~/ listEnd).map(xs => NBTIntArray(xs.map(_.toInt).toVector))

    def indexedTag[_: P]: P[IndexedTag] = P(tagIndex ~/ colon ~/ nbtTag)
    def nbtList[_: P]: P[NBTList[_, _ <: NBTTag]] =
      P(listStart ~/ indexedTag.rep(sep = comma./) ~/ listEnd)
        .filter {
          case seq if seq.nonEmpty =>
            val head   = seq.head._2
            val sameId = seq.forall(a => a._2.nbtType.id == head.nbtType.id)

            sameId && seq.map(_._1) == seq.indices
          case _ => true
        }
        .map {
          case seq if seq.nonEmpty =>
            val mapped   = seq.map(_._2)
            val head     = mapped.head
            val nbtType  = new NBTListType(head.nbtType.asInstanceOf[unsafe.AnyTagType])
            val withType = mapped.asInstanceOf[Seq[unsafe.AnyTag]]

            NBTList[Any, unsafe.AnyTag](withType)(nbtType)
          case _ =>
            NBTList[Byte, NBTByte]().asInstanceOf[NBTList[Any, unsafe.AnyTag]] //We use byte if there are no elements
        }
        .opaque("NBT List")

    def wholeNbt[_: P]: P[NBTCompound] = P(nbtCompound ~ End)
  }

  /**
		* Convert a [[net.katsstuff.typenbt.NBTTag]] to mojangson.
		*/
  def serialize(tag: NBTTag): String = tag match {
    case NBTByte(b)          => s"${b}b"
    case NBTShort(s)         => s"${s}s"
    case NBTInt(i)           => s"$i"
    case NBTLong(l)          => s"${l}L"
    case NBTFloat(f)         => s"${f}f"
    case NBTDouble(d)        => s"${d}d"
    case NBTByteArray(array) => s"[${array.length} bytes]"
    case NBTString(s)        => s""""$s""""
    case NBTList(list)       => toMojangsonList(list)
    case NBTCompound(tags)   => toMojangsonCompound(tags)
    case NBTIntArray(ints)   => toMojangsonIntArray(ints)
  }

  private def toMojangsonIterable[A](startChar: Char, endChar: Char, xs: Iterable[A])(format: A => String) = {
    val b = new StringBuilder(s"$startChar")

    for (x <- xs) {
      if (b.length != 1) {
        b.append(',')
      }
      b.append(format(x))
    }

    b.append(endChar).mkString
  }

  private def toMojangsonList(list: Seq[NBTTag]): String = toMojangsonIterable('[', ']', list.zipWithIndex) {
    case (tag, index) => s"$index:${serialize(tag)}"
  }
  private def toMojangsonCompound(tags: Map[String, NBTTag]): String = toMojangsonIterable('{', '}', tags) {
    case (name, tag) => s"$name:${serialize(tag)}"
  }
  private def toMojangsonIntArray(ints: Seq[Int]): String = toMojangsonIterable('[', ']', ints)(int => s"$int")

  /**
		* Convert a [[net.katsstuff.typenbt.NBTTag]] to mojangson with indentation
		*
		* @param tag The tag to convert
		* @param indentLevel How many indent characters to insert per level
		* @param indentChar The indent character to use
		*/
  def serializeIndent(tag: NBTTag, indentLevel: Int = 1, indentChar: Char = '	'): String = {
    tag match {
      case NBTList(list)     => toMojangsonIndentList(list, indentLevel, indentChar)
      case NBTCompound(tags) => toMojangsonIndentCompound(tags, indentLevel, indentChar)
      case NBTIntArray(tags) => toMojangsonIndentIntArray(tags, indentLevel, indentChar)
      case _                 => serialize(tag)
    }
  }

  private def indent(b: StringBuilder, indentLevel: Int, indentChar: Char): Unit = {
    b.append('\n')
    (0 to indentLevel).foreach(_ => b.append(indentChar))
  }

  private def toMojangsonIndentIterable[A](
      startChar: Char,
      endChar: Char,
      indentLevel: Int,
      indentChar: Char,
      xs: Iterable[A]
  )(format: A => String): String = {
    val b = new StringBuilder(s"$startChar")

    for (x <- xs) {
      if (b.length != 1) {
        b.append(',')
      }
      indent(b, indentLevel, indentChar)
      b.append(format(x))
    }

    if (xs.nonEmpty) {
      indent(b, indentLevel - 1, indentChar)
    }
    b.append(endChar).mkString
  }

  private def toMojangsonIndentList(list: Seq[NBTTag], indentLevel: Int, indentChar: Char): String =
    toMojangsonIndentIterable('[', ']', indentLevel, indentChar, list.zipWithIndex) {
      case (tag, index) => s"$index:${serializeIndent(tag, indentLevel, indentChar)}"
    }
  private def toMojangsonIndentCompound(tags: Map[String, NBTTag], indentLevel: Int, indentChar: Char): String =
    toMojangsonIndentIterable('{', '}', indentLevel, indentChar, tags) {
      case (name, tag) => s"$name:${serializeIndent(tag, indentLevel, indentChar)}"
    }
  private def toMojangsonIndentIntArray(ints: Seq[Int], indentLevel: Int, indentChar: Char): String =
    toMojangsonIndentIterable('[', ']', indentLevel, indentChar, ints)(int => s"$int")
}
