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

import scala.util.matching.Regex

import fastparse.WhitespaceApi
import fastparse.core.{Mutable, ParseCtx}
import fastparse.noApi._
import net.katsstuff.typenbt.nbt._

object Mojangson {

  /**
		* Parse mojangson into a [[net.katsstuff.typenbt.nbt.NBTTag]]
		*/
  def fromMojangson(mojangson: String): Either[String, NBTTag] = MojangsonParser.wholeNbt.parse(mojangson) match {
    case Parsed.Success(unknownNbt, _) => Right(unknownNbt)
    case f @ Parsed.Failure(_, _, _)   => Left(f.msg)
  }

  object MojangsonParser {

    private case class RegexParser(regex: Regex, maxLength: Int = 80)
        extends fastparse.core.Parser[Unit, Char, String]()(fastparse.StringReprOps) /*ambigous implicit*/ {
      override def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[Unit, Char, String] =
        regex.findPrefixOf(cfg.input.slice(index, maxLength)) match {
          case Some(parsed) => success(cfg.success, (), index + reprOps.length(parsed), Set.empty, cut = false)
          case None         => fail(cfg.failure, index)
        }
    }

    val White = WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace(CharPred(Character.isWhitespace).rep)
    }

    import White._

    type NamedTag   = (String, NBTTag)
    type IndexedTag = (Int, NBTTag)
    type AnyTag     = NBTView.AnyTag.NBT

    val stringLiteral: Parser[String] = P(RegexParser("""\"(\\.|[^\\"])*\"""".r)).!.map(s => s.replace("""\"""", """"""").replace("""\\""", """\"""))
    val floatingPoint: Parser[Double] = P(RegexParser("""[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""".r)).!.map(_.toDouble)
    val wholeNumber:   Parser[Long]   = P("-".? ~ CharIn('0' to '9').rep(1)).!.map(_.toLong)

    val colon:    Parser[Unit]   = P(":")
    val comma:    Parser[Unit]   = P(",")
    val tagName:  Parser[String] = P(CharsWhile(c => !":{}[]".contains(c)).!) //Better way?
    val tagIndex: Parser[Int]    = P(CharIn('0' to '9').rep(1).!).map(_.toInt)

    val compoundStart: Parser[Unit] = P("{")
    val compoundEnd:   Parser[Unit] = P("}")
    val listStart:     Parser[Unit] = P("[")
    val listEnd:       Parser[Unit] = P("]")

    val byteEnd:   Parser[Unit] = P("b")
    val shortEnd:  Parser[Unit] = P("s")
    val longEnd:   Parser[Unit] = P("L")
    val floatEnd:  Parser[Unit] = P(CharIn("fF"))
    val doubleEnd: Parser[Unit] = P(CharIn("dD"))

    val nbtByte:   Parser[NBTByte]   = P(wholeNumber ~ byteEnd).map(numb => NBTByte(numb.toByte))
    val nbtShort:  Parser[NBTShort]  = P(wholeNumber ~ shortEnd).map(numb => NBTShort(numb.toShort))
    val nbtLong:   Parser[NBTLong]   = P(wholeNumber ~ longEnd).map(numb => NBTLong(numb))
    val nbtFloat:  Parser[NBTFloat]  = P(floatingPoint ~ floatEnd).map(numb => NBTFloat(numb.toFloat))
    val nbtDouble: Parser[NBTDouble] = P(floatingPoint ~ doubleEnd).map(numb => NBTDouble(numb))
    val nbtInt:    Parser[NBTInt]    = wholeNumber.map(numb => NBTInt(numb.toInt))

    val nbtNumber: Parser[NBTTag]    = P(nbtByte | nbtShort | nbtLong | nbtFloat | nbtDouble | nbtInt)
    val nbtString: Parser[NBTString] = stringLiteral.map(s => NBTString(s.substring(1, s.length - 1)))

    //We make this lazy so that there won't be any wrong forward references
    lazy val nbtTag: Parser[NBTTag] = P(nbtNumber | nbtString | nbtCompound | NoCut(nbtList) | nbtIntArray)

    val nbtNamedTag: Parser[(String, NBTTag)] = P(tagName ~ colon ~ nbtTag)
    val nbtCompound: Parser[NBTCompound]      = P(compoundStart ~/ nbtNamedTag.rep(sep = comma.~/) ~ compoundEnd).map(xs => NBTCompound(xs.toMap))
    val nbtIntArray: Parser[NBTIntArray]      = P(listStart ~/ wholeNumber.rep(sep = comma.~/) ~ listEnd).map(xs => NBTIntArray(xs.map(_.toInt).toVector))

    val indexedTag: Parser[(Int, NBTTag)] = P(tagIndex ~ colon ~ nbtTag)
    val nbtList = P(listStart ~/ indexedTag.rep(sep = comma.~/) ~ listEnd)
      .filter({
        case seq if seq.nonEmpty =>
          val head     = seq.head._2
          val sameId   = seq.forall(a => a._2.nbtType.id == head.nbtType.id)
          val indicies = seq.foldLeft(0) { case (prev, (i, _)) => if (i == prev) prev + 1 else prev }

          sameId && indicies == seq.size
        case _ => true
      })
      .map({
        case seq if seq.nonEmpty =>
          NBTList[Any, AnyTag](seq.map(_._2).asInstanceOf[Seq[AnyTag]])(NBTView.TAG_LIST, seq.head._2.nbtType.asInstanceOf[NBTType.Aux[Any, AnyTag]])
        case _ => NBTList[Byte, NBTByte](Seq()).asInstanceOf[NBTList[Any, AnyTag]] //We use byte if there are no elements
      })

    val wholeNbt = P(nbtCompound ~ End)
  }

  /**
		* Convert a [[net.katsstuff.typenbt.nbt.NBTTag]] to mojangson.
		*/
  def toMojangson(tag: NBTTag): String = tag match {
    case NBTByte(b)          => s"${b}b"
    case NBTShort(s)         => s"${s}s"
    case NBTInt(i)           => s"$i"
    case NBTLong(l)          => s"${l}L"
    case NBTFloat(f)         => s"${f}f"
    case NBTDouble(d)        => s"${d}d"
    case NBTByteArray(array) => s"[${array.length} bytes]"
    case NBTString(s)        => s""""$s""""
    case NBTList(list) =>
      val b = new StringBuilder("[")

      for ((tag, index) <- list.zipWithIndex) {
        if (index != 0) {
          b.append(',')
        }
        b.append(s"$index:${toMojangson(tag)}")
      }

      b.append(']').mkString
    case NBTCompound(tags) =>
      val b = new StringBuilder("{")

      for (((name, tag)) <- tags) {
        if (b.length != 1) {
          b.append(',')
        }
        b.append(s"$name:${toMojangson(tag)}")
      }

      b.append('}').mkString
    case NBTIntArray(array) =>
      val b = new StringBuilder("[")

      for (i <- array) {
        if (b.length != 1) {
          b.append(',')
        }
        b.append(s"$i")
      }

      b.append(']').mkString
  }

  /**
		* Convert a [[net.katsstuff.typenbt.nbt.NBTTag]] to mojangson with indentation
		*
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

        for ((tag, index) <- list.zipWithIndex) {
          if (index != 0) {
            b.append(',')
          }
          indent(b, indentLevel)
          b.append(s"$index:${toMojangsonIndent(tag, indentLevel + 1, indentChar)}")
        }

        if (list.nonEmpty) {
          indent(b, indentLevel - 1)
        }
        b.append(']').mkString
      case NBTCompound(tags) =>
        val b = new StringBuilder("{")

        for (((name, tag)) <- tags) {
          if (b.length != 1) {
            b.append(',')
          }

          indent(b, indentLevel)
          b.append(s"$name:${toMojangsonIndent(tag, indentLevel + 1, indentChar)}")
        }

        if (tags.nonEmpty) {
          indent(b, indentLevel - 1)
        }

        b.append('}').mkString
      case NBTIntArray(tags) =>
        val b = new StringBuilder("[")

        for (tag <- tags) {
          if (b.size != 1) {
            b.append(',')
          }
          indent(b, indentLevel)
          b.append(s"$tag")
        }

        if (tags.nonEmpty) {
          indent(b, indentLevel - 1)
        }
        b.append(']').mkString
      case _ => toMojangson(tag)
    }
  }
}
