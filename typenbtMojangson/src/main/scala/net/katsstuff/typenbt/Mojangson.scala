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

import cats.parse.{Numbers, Parser0, Rfc5234, Parser => P}

object Mojangson {

  /** Parse mojangson into a [[net.katsstuff.typenbt.NBTTag]] */
  def deserialize(mojangson: String): Either[P.Error, NBTCompound] =
    MojangsonParser.wholeNbt.parseAll(mojangson)

  object MojangsonParser {

    type NamedTag   = (String, NBTTag)
    type IndexedTag = (Option[Int], NBTTag)

    val optionalWhitespace: Parser0[Unit] = (Rfc5234.wsp | Rfc5234.cr | Rfc5234.lf).rep0.void

    def withOptionalWhitespace[A](p: P[A]): P[A] =
      optionalWhitespace.soft.with1 *> p <* optionalWhitespace

    // Represents the regex \"(\\.|[^\\"])*\"
    def stringLiteral(quotes: Char): P[String] =
      ((P.char('\\') ~ P.anyChar) | P.charWhere(c => c != '\\' && c != quotes)).rep0.with1
        .surroundedBy(P.char(quotes))
        .string
        .map(_.replace("\\" + quotes, quotes.toString).replace("\\\\", "\\"))
        .withContext("String literal")

    val rawString: P[String] =
      ((Rfc5234.alpha | Numbers.digit).rep0.with1 ~ (P.char(' ') ~ P.charsWhile(c =>
        c != ',' && c != ']' && c != '}'
      ))).string

    val floatingPoint: P[BigDecimal] = {
      val sign        = P.charIn("+-").?
      val fraction    = P.char('.') ~ Numbers.digits
      val exponential = P.charIn("eE") ~ sign ~ Numbers.digits

      (sign.with1 ~ fraction.orElse(Numbers.digits ~ fraction.?) ~ exponential.?).string
        .map(BigDecimal(_))
        .withContext("Floating point number")
    }

    val zNumber: P[BigInt] = Numbers.bigInt.withContext("Whole number")

    val colon: P[Unit]     = withOptionalWhitespace(P.char(':')).void
    val comma: P[Unit]     = withOptionalWhitespace(P.char(',')).void
    val tagName: P[String] = withOptionalWhitespace(P.charsWhile(c => !":{}[]".contains(c))).withContext("Tag name") // Better way?
    val tagIndex: P[Int]   = withOptionalWhitespace(zNumber.map(_.toInt)).withContext("Tag index")

    val compoundStart: P[Unit] = withOptionalWhitespace(P.char('{')).void.withContext("Compound start")
    val compoundEnd: P[Unit]   = withOptionalWhitespace(P.char('}')).void.withContext("Compound end")
    val listStart: P[Unit]     = withOptionalWhitespace(P.char('[')).void.withContext("List start")
    val listEnd: P[Unit]       = withOptionalWhitespace(P.char(']')).void.withContext("List end")

    val byteEnd: P[Unit]   = P.charIn("bB").void.withContext("Byte end")
    val shortEnd: P[Unit]  = P.charIn("sS").void.withContext("Short end")
    val longEnd: P[Unit]   = P.charIn("lL").void.withContext("Long end")
    val floatEnd: P[Unit]  = P.charIn("fF").void.withContext("Float end")
    val doubleEnd: P[Unit] = P.charIn("dD").void.withContext("Double end")

    val nbtByte: P[NBTByte]   = (zNumber <* byteEnd).map(n => NBTByte(n.toByte))
    val nbtShort: P[NBTShort] = (zNumber <* shortEnd).map(n => NBTShort(n.toShort))
    val nbtLong: P[NBTLong]   = (zNumber <* longEnd).map(n => NBTLong(n.toLong))
    val nbtFloat: P[NBTFloat] = (floatingPoint <* floatEnd).map(n => NBTFloat(n.toFloat))
    val nbtDouble: P[NBTDouble] =
      (floatingPoint ~ doubleEnd.?)
        .filter { case (n, end) =>
          end.isDefined || !n.isWhole
        }
        .map(t => NBTDouble(t._1.toDouble))

    val nbtInt: P[NBTInt] = zNumber.map(n => NBTInt(n.toInt))

    val nbtNumber: P[NBTTag] =
      nbtByte.backtrack | nbtShort.backtrack | nbtLong.backtrack | nbtFloat.backtrack | nbtDouble.backtrack | nbtInt
    val nbtString: P[NBTString] =
      (rawString | stringLiteral('"') | stringLiteral('\'')).map(s => NBTString(s.substring(1, s.length - 1)))

    val nbtByteArray: P[NBTByteArray] = zNumber
      .repSep0(comma)
      .with1
      .between(listStart ~ P.string("B;"), listEnd)
      .map(xs => NBTByteArray(xs.map(_.toByte).toVector))
    val nbtIntArray: P[NBTIntArray] = zNumber
      .repSep0(comma)
      .with1
      .between(listStart ~ P.string("I;"), listEnd)
      .map(xs => NBTIntArray(xs.map(_.toInt).toVector))
    val nbtLongArray: P[NBTLongArray] = zNumber
      .repSep0(comma)
      .with1
      .between(listStart ~ P.string("L;"), listEnd)
      .map(xs => NBTLongArray(xs.map(_.toLong).toVector))

    val nbtArray: P[NBTTag] = nbtByteArray.backtrack | nbtIntArray.backtrack | nbtLongArray.backtrack

    lazy val nbtArrayIsh: P[NBTTag] = P.defer(nbtArray | nbtList)

    lazy val nbtTag: P[NBTTag] = P.defer(nbtNumber | nbtString | nbtCompound | nbtArrayIsh)

    val nbtNamedTag: P[NamedTag] = (tagName <* colon.void) ~ nbtTag
    val nbtCompound: P[NBTCompound] =
      nbtNamedTag.repSep0(comma).with1.between(compoundStart, compoundEnd).map(xs => NBTCompound(xs.toMap))

    val indexedTag: P[IndexedTag] = (tagIndex.soft <* colon).?.with1 ~ nbtTag
    val nbtList: P[NBTList[_, _ <: NBTTag]] =
      indexedTag
        .repSep0(comma)
        .with1
        .between(listStart, listEnd)
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
            NBTList[Byte, NBTByte]().asInstanceOf[NBTList[Any, unsafe.AnyTag]] // We use byte if there are no elements
        }
        .withContext("NBT List")

    val wholeNbt: P[NBTCompound] = nbtCompound <* P.end
  }

  /** Convert a [[net.katsstuff.typenbt.NBTTag]] to mojangson. */
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
      toMojangsonIterable("[", "]", list.zipWithIndex) { case (nbt, index) =>
        s"$index:${serialize(nbt, indexedList)}"
      }
    } else {
      toMojangsonIterable("[", "]", list)(serialize(_, indexedList))
    }

  private def toMojangsonCompound(tags: Map[String, NBTTag], indexedList: Boolean): String =
    toMojangsonIterable("{", "}", tags) { case (name, tag) =>
      s"$name:${serialize(tag, indexedList)}"
    }

  private def toMojangsonArray(arrayPrefix: String, values: Seq[_]): String =
    toMojangsonIterable(s"[$arrayPrefix;", "]", values)(_.toString)

  /**
    * Convert a [[net.katsstuff.typenbt.NBTTag]] to mojangson with indentation
    *
    * @param tag
    *   The tag to convert
    * @param indentLevel
    *   How many indent characters to insert per level
    * @param indentChar
    *   The indent character to use
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
      toMojangsonIndentIterable("[", "]", indentLevel, indentChar, list.zipWithIndex) { case (tag, index) =>
        s"$index:${serializeIndent(tag, indentLevel, indentChar, indexedList)}"
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
    toMojangsonIndentIterable("{", "}", indentLevel, indentChar, tags) { case (name, tag) =>
      s"$name:${serializeIndent(tag, indentLevel, indentChar, indexedList)}"
    }

  private def toMojangsonIndentArray(values: Seq[_], prefix: String, indentLevel: Int, indentChar: Char): String =
    toMojangsonIndentIterable(s"[$prefix;", "]", indentLevel, indentChar, values)(_.toString)
}
