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
  def deserialize(mojangson: String, verbose: Boolean = false): Parsed[NBTCompound] =
    parse(mojangson, MojangsonParser.wholeNbt(_), verbose)

  object MojangsonParser {

    type NamedTag   = (String, NBTTag)
    type IndexedTag = (Option[Int], NBTTag)

    def nNumber[_: P]: P[Unit] = P(CharsWhileIn("0-9"))

    // Represents the regex \"(\\.|[^\\"])*\"
    def stringLiteral[_: P](quotes: Char): P[String] =
      P(
        quotes.toString ~ (("\\" ~ AnyChar) | CharPred(c => c != '\\' && c != quotes)).rep ~ quotes.toString
      ).!.map(_.replace("\\" + quotes, quotes.toString).replace("\\\\", "\\")).opaque("String literal")

    def rawString[_: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9") ~ (" " ~ CharsWhile(c => c != ',' && c != ']' && c != '}', 1))).!

    // Represents the regex [-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?
    def floatingPoint[_: P]: P[BigDecimal] =
      P(
        CharIn("+\\-").? ~ (&(CharsWhileIn("0-9") ~ ".") ~ CharsWhileIn("0-9")).? ~ ".".? ~ nNumber ~ (CharIn("eE") ~ CharIn(
          "+\\-"
        ).? ~ nNumber).?
      ).!.map(BigDecimal(_)).opaque("Floating point number")

    def zNumber[_: P]: P[BigInt] = P("-".? ~ nNumber).!.map(BigInt(_)).opaque("Whole number")

    def colon[_: P]: P[Unit]     = P(":")
    def comma[_: P]: P[Unit]     = P(",")
    def tagName[_: P]: P[String] = P(CharsWhile(c => !":{}[]".contains(c))).!.opaque("Tag name") //Better way?
    def tagIndex[_: P]: P[Int]   = P(nNumber).!.map(_.toInt).opaque("Tag index")

    def compoundStart[_: P]: P[Unit] = P("{").opaque("Compound start")
    def compoundEnd[_: P]: P[Unit]   = P("}").opaque("Compound end")
    def listStart[_: P]: P[Unit]     = P("[").opaque("List start")
    def listEnd[_: P]: P[Unit]       = P("]").opaque("List end")

    def byteEnd[_: P]: P[Unit]   = P(CharIn("bB")).opaque("Byte end")
    def shortEnd[_: P]: P[Unit]  = P(CharIn("sS")).opaque("Short end")
    def longEnd[_: P]: P[Unit]   = P(CharIn("lL")).opaque("Long end")
    def floatEnd[_: P]: P[Unit]  = P(CharIn("fF")).opaque("Float end")
    def doubleEnd[_: P]: P[Unit] = P(CharIn("dD")).opaque("Double end")

    def nbtByte[_: P]: P[NBTByte]   = P(zNumber ~ byteEnd).map(n => NBTByte(n.toByte))
    def nbtShort[_: P]: P[NBTShort] = P(zNumber ~ shortEnd).map(n => NBTShort(n.toShort))
    def nbtLong[_: P]: P[NBTLong]   = P(zNumber ~ longEnd).map(n => NBTLong(n.toLong))
    def nbtFloat[_: P]: P[NBTFloat] = P(floatingPoint ~ floatEnd).map(n => NBTFloat(n.toFloat))
    def nbtDouble[_: P]: P[NBTDouble] =
      P(
        (zNumber.map(BigDecimal(_)) ~ doubleEnd) |
          (zNumber ~ floatingPoint ~ doubleEnd.?)
            .map(t => BigDecimal(t._1) + (math.signum(t._1.toInt) * t._2)) | //Parse whole part followed by floating part
          (!zNumber ~ floatingPoint ~ doubleEnd.?) //Only used for more exotic stuff
      ).map(n => NBTDouble(n.toDouble))
    def nbtInt[_: P]: P[NBTInt] = P(zNumber.map(n => NBTInt(n.toInt)))

    def nbtNumber[_: P]: P[NBTTag] = P(nbtByte | nbtShort | nbtLong | nbtFloat | nbtDouble | nbtInt)
    def nbtString[_: P]: P[NBTString] =
      P(rawString | stringLiteral('"') | stringLiteral('\'')).map(s => NBTString(s.substring(1, s.length - 1)))

    def nbtTag[_: P]: P[NBTTag] =
      P(nbtNumber | nbtString | nbtCompound | nbtByteArray | nbtIntArray | nbtLongArray | nbtList)

    def nbtNamedTag[_: P]: P[NamedTag] = P(tagName ~/ colon ~/ nbtTag)
    def nbtCompound[_: P]: P[NBTCompound] =
      P(compoundStart ~/ nbtNamedTag.rep(sep = comma./) ~/ compoundEnd).map(xs => NBTCompound(xs.toMap))
    def nbtByteArray[_: P]: P[NBTByteArray] =
      P(listStart ~ "B;" ~/ zNumber.rep(sep = comma./) ~/ listEnd).map(xs => NBTByteArray(xs.map(_.toByte).toVector))
    def nbtIntArray[_: P]: P[NBTIntArray] =
      P(listStart ~ "I;" ~/ zNumber.rep(sep = comma./) ~/ listEnd).map(xs => NBTIntArray(xs.map(_.toInt).toVector))
    def nbtLongArray[_: P]: P[NBTLongArray] =
      P(listStart ~ "L;" ~/ zNumber.rep(sep = comma./) ~/ listEnd).map(xs => NBTLongArray(xs.map(_.toLong).toVector))

    def indexedTag[_: P]: P[IndexedTag] = P((tagIndex ~ colon).? ~ nbtTag)
    def nbtList[_: P]: P[NBTList[_, _ <: NBTTag]] =
      P(listStart ~ indexedTag.rep(sep = comma./) ~/ listEnd)
      /*
        .filter {
          case seq if seq.nonEmpty =>
            val head   = seq.head._2
            val sameId = seq.forall(a => a._2.nbtType.id == head.nbtType.id)

            val stringIndices      = seq.map(_._1)
            val missingSomeIndices = stringIndices.exists(_.isEmpty)

            sameId && (missingSomeIndices || stringIndices == seq.indices)
          case _ => true
        }
         */
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
  def serialize(tag: NBTTag, indexedList: Boolean = false): String = tag match {
    case NBTByte(b)          => s"${b}b"
    case NBTShort(s)         => s"${s}s"
    case NBTInt(i)           => s"$i"
    case NBTLong(l)          => s"${l}L"
    case NBTFloat(f)         => s"${f}f"
    case NBTDouble(d)        => s"${d}d"
    case NBTByteArray(array) => toMojangsonArray("B", array)
    case NBTIntArray(ints)   => toMojangsonArray("I", ints)
    case NBTLongArray(longs) => toMojangsonArray("L", longs)
    case NBTString(s)        => s""""$s""""
    case NBTList(list)       => toMojangsonList(list, indexedList)
    case NBTCompound(tags)   => toMojangsonCompound(tags, indexedList)
  }

  private def toMojangsonIterable[A](start: String, end: String, xs: Iterable[A])(format: A => String) = {
    val b = new StringBuilder(start)

    var atStart = true
    for (x <- xs) {
      if (!atStart) {
        b.append(',')
      }

      atStart = false
      b.append(format(x))
    }

    b.append(end).mkString
  }

  private def toMojangsonList(list: Seq[NBTTag], indexedList: Boolean): String =
    if (indexedList) {
      toMojangsonIterable("[", "]", list.zipWithIndex) {
        case (nbt, index) => s"$index:${serialize(nbt, indexedList)}"
      }
    } else {
      toMojangsonIterable("[", "]", list)(serialize(_, indexedList))
    }

  private def toMojangsonCompound(tags: Map[String, NBTTag], indexedList: Boolean): String =
    toMojangsonIterable("{", "}", tags) {
      case (name, tag) => s"$name:${serialize(tag, indexedList)}"
    }

  private def toMojangsonArray(arrayPrefix: String, values: Seq[_]): String =
    toMojangsonIterable(s"[$arrayPrefix;", "]", values)(_.toString)

  /**
		* Convert a [[net.katsstuff.typenbt.NBTTag]] to mojangson with indentation
		*
		* @param tag The tag to convert
		* @param indentLevel How many indent characters to insert per level
		* @param indentChar The indent character to use
		*/
  def serializeIndent(tag: NBTTag, indentLevel: Int = 1, indentChar: Char = '	', indexedList: Boolean): String = {
    tag match {
      case NBTList(list)      => toMojangsonIndentList(list, indentLevel, indentChar, indexedList)
      case NBTCompound(tags)  => toMojangsonIndentCompound(tags, indentLevel, indentChar, indexedList)
      case NBTByteArray(tags) => toMojangsonIndentArray(tags, "B", indentLevel, indentChar)
      case NBTIntArray(tags)  => toMojangsonIndentArray(tags, "I", indentLevel, indentChar)
      case NBTLongArray(tags) => toMojangsonIndentArray(tags, "L", indentLevel, indentChar)
      case _                  => serialize(tag)
    }
  }

  private def indent(b: StringBuilder, indentLevel: Int, indentChar: Char): Unit = {
    b.append('\n')
    (0 to indentLevel).foreach(_ => b.append(indentChar))
  }

  private def toMojangsonIndentIterable[A](
      start: String,
      end: String,
      indentLevel: Int,
      indentChar: Char,
      xs: Iterable[A]
  )(format: A => String): String = {
    val b = new StringBuilder(start)

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
    b.append(end).mkString
  }

  private def toMojangsonIndentList(
      list: Seq[NBTTag],
      indentLevel: Int,
      indentChar: Char,
      indexedList: Boolean
  ): String =
    if (indexedList) {
      toMojangsonIndentIterable("[", "]", indentLevel, indentChar, list.zipWithIndex) {
        case (tag, index) => s"$index:${serializeIndent(tag, indentLevel, indentChar, indexedList)}"
      }
    } else {
      toMojangsonIndentIterable("[", "]", indentLevel, indentChar, list)(
        serializeIndent(_, indentLevel, indentChar, indexedList)
      )
    }

  private def toMojangsonIndentCompound(
      tags: Map[String, NBTTag],
      indentLevel: Int,
      indentChar: Char,
      indexedList: Boolean
  ): String =
    toMojangsonIndentIterable("{", "}", indentLevel, indentChar, tags) {
      case (name, tag) => s"$name:${serializeIndent(tag, indentLevel, indentChar, indexedList)}"
    }

  private def toMojangsonIndentArray(values: Seq[_], prefix: String, indentLevel: Int, indentChar: Char): String =
    toMojangsonIndentIterable(s"[$prefix;", "]", indentLevel, indentChar, values)(_.toString)
}
