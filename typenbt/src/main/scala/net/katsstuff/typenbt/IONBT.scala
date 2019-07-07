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

import java.io._
import java.nio.charset.StandardCharsets
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object IONBT {

  final private val UTF8 = StandardCharsets.UTF_8

  /**
		* Writes an [[net.katsstuff.typenbt.NBTCompound]] to an [[java.io.OutputStream]]
		*
		* @param stream The stream to write to
		* @param compound The tag to write out
		* @param rootName The name of the root of the NBT. Usually blank.
		* @param gzip If the stream should be Gziped or not
		*/
  def write(stream: OutputStream, compound: NBTCompound, rootName: String = "", gzip: Boolean): Try[Unit] = {
    val newStream = new DataOutputStream(
      if (gzip) new BufferedOutputStream(new GZIPOutputStream(stream))
      else stream
    )

    val ret = for {
      _ <- writeType(newStream, compound.nbtType)
      _ <- writeString(newStream, rootName)
      _ <- writeCompound(newStream, compound)
    } yield ()

    newStream.close()
    ret
  }

  /**
		* Reads an [[net.katsstuff.typenbt.NBTCompound]] from an [[java.io.InputStream]]
		*
		* @param stream The stream to read from
		* @param gzip If the [[net.katsstuff.typenbt.NBTCompound]] to read from the stream is GZiped or not
		* @return A tuple compromising of the [[net.katsstuff.typenbt.NBTCompound]] read, as well as the root name
		*/
  def read(stream: InputStream, gzip: Boolean): Try[(String, NBTCompound)] = {
    val newStream = new DataInputStream(
      if (gzip) new BufferedInputStream(new GZIPInputStream(stream))
      else stream
    )

    val ret = readType(newStream).flatMap {
      case tpe if tpe == NBTType.TagCompound =>
        for {
          name <- readString(newStream)
          tag  <- readCompound(newStream, NBTCompound())
        } yield (name, tag)
      case _ => Failure(new IOException("Wrong starting type for NBT"))
    }

    newStream.close()
    ret
  }

  private def writeEndTag(stream: DataOutputStream): Try[Unit] = Try(stream.writeByte(0))

  private def writeCompound(stream: DataOutputStream, nbt: NBTCompound): Try[Unit] = {

    @tailrec
    def inner(remaining: Seq[(String, NBTTag)])(prev: Try[Unit]): Try[Unit] =
      if (remaining.isEmpty) {
        for {
          _ <- prev
          _ <- writeEndTag(stream)
        } yield ()
      } else {
        inner(remaining.tail) {
          val (name, tag) = remaining.head
          for {
            _ <- prev
            _ <- writeType(stream, tag.nbtType)
            _ <- writeString(stream, name)
            _ <- writeTag(stream, tag)
          } yield ()
        }
      }

    inner(nbt.value.toSeq)(Success(()))
  }

  private def writeString(stream: DataOutputStream, string: String): Try[Unit] =
    for {
      _ <- Try(stream.writeShort(string.length))
      _ <- Try(stream.write(string.getBytes(UTF8)))
    } yield ()

  private def writeList(stream: DataOutputStream, list: NBTList[_, _ <: NBTTag]): Try[Unit] = {
    val ret = for {
      _ <- writeType(stream, list.nbtType.elementType)
      _ <- Try(stream.writeInt(list.value.size))
    } yield ()

    list.value.foldLeft(ret) {
      case (Success(_), tag)   => writeTag(stream, tag)
      case (f @ Failure(_), _) => f
    }
  }

  private def writeByteArray(stream: DataOutputStream, array: Array[Byte]): Try[Unit] =
    for {
      _ <- Try(stream.writeInt(array.length))
      _ <- Try(stream.write(array))
    } yield ()

  private def writeIntArray(stream: DataOutputStream, array: Array[Int]): Try[Unit] =
    array.foldLeft(Try(stream.writeInt(array.length))) {
      case (Success(_), int)   => Try(stream.writeInt(int))
      case (f @ Failure(_), _) => f
    }

  private def writeLongArray(stream: DataOutputStream, array: Array[Long]): Try[Unit] =
    array.foldLeft(Try(stream.writeInt(array.length))) {
      case (Success(_), long)  => Try(stream.writeLong(long))
      case (f @ Failure(_), _) => f
    }

  private def writeType(stream: DataOutputStream, tagType: NBTType[_, _ <: NBTTag]): Try[Unit] =
    Try(stream.writeByte(tagType.id))

  private def writeTag(stream: DataOutputStream, nbt: NBTTag): Try[Unit] =
    nbt match {
      case NBTByte(b)              => Try(stream.writeByte(b))
      case NBTShort(s)             => Try(stream.writeShort(s))
      case NBTInt(i)               => Try(stream.writeInt(i))
      case NBTLong(l)              => Try(stream.writeLong(l))
      case NBTFloat(f)             => Try(stream.writeFloat(f))
      case NBTDouble(d)            => Try(stream.writeDouble(d))
      case NBTByteArray(array)     => writeByteArray(stream, array.toArray)
      case NBTString(s)            => writeString(stream, s)
      case list: NBTList[_, _]     => writeList(stream, list)
      case compound: NBTCompound   => writeCompound(stream, compound)
      case NBTIntArray(intArray)   => writeIntArray(stream, intArray.toArray)
      case NBTLongArray(longArray) => writeLongArray(stream, longArray.toArray)
    }

  @tailrec
  private def readCompound(stream: DataInputStream, compound: NBTCompound): Try[NBTCompound] = {
    //We match to be tail recursive
    readType(stream) match {
      case Success(nbtType) if nbtType == NBTType.TagEnd => Success(compound)
      case Success(nbtType) =>
        readString(stream) match {
          case Success(name) =>
            readTag(stream, nbtType) match {
              case Success(tag)                   => readCompound(stream, compound.set(name, tag))
              case f: Failure[Nothing @unchecked] => f
            }
          case f: Failure[Nothing @unchecked] => f
        }
      case f: Failure[Nothing @unchecked] => f
    }
  }

  private def readString(stream: DataInputStream): Try[String] =
    for {
      length <- Try(stream.readShort())
      characters = new Array[Byte](length)
      _ <- Try(stream.readFully(characters))
    } yield new String(characters, UTF8)

  private def readList(stream: DataInputStream): Try[NBTList[Any, unsafe.AnyTag]] =
    for {
      nbtType <- readType(stream)
      listType = new NBTListType(nbtType)
      length <- Try(stream.readInt())
      res <- (0 until length).foldLeft(Try(NBTList()(listType))) {
        case (Success(list), _)  => readTag(stream, nbtType).map(read => list :+ read)
        case (f @ Failure(_), _) => f
      }
    } yield res

  private def readByteArray(stream: DataInputStream): Try[Array[Byte]] =
    for {
      length <- Try(stream.readInt())
      bytes = new Array[Byte](length)
      _ <- Try(stream.readFully(bytes))
    } yield bytes

  private def readIntArray(stream: DataInputStream): Try[Array[Int]] =
    for {
      length <- Try(stream.readInt())
      array = new Array[Int](length)
      _ <- (0 until length).foldLeft[Try[Unit]](Success(())) {
        case (Success(_), i)     => Try(array(i) = stream.readInt())
        case (f @ Failure(_), _) => f
      }
    } yield array

  private def readLongArray(stream: DataInputStream): Try[Array[Long]] =
    for {
      length <- Try(stream.readInt())
      array = new Array[Long](length)
      _ <- (0 until length).foldLeft[Try[Unit]](Success(())) {
        case (Success(_), i)     => Try(array(i) = stream.readLong())
        case (f @ Failure(_), _) => f
      }
    } yield array

  private def readType(stream: DataInputStream): Try[unsafe.AnyTagType] = Try {
    val byte = stream.readByte()
    NBTType
      .fromId(byte)
      .getOrElse(throw new IOException(s"Read type $byte on NBT is not valid"))
      .asInstanceOf[unsafe.AnyTagType]
  }

  private def readTag[A](stream: DataInputStream, nbtType: NBTType.CovarObj[A]): Try[NBTTag.Aux[A]] =
    nbtType match {
      case NBTType.TagByte      => Try(NBTByte(stream.readByte()).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagShort     => Try(NBTShort(stream.readShort()).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagInt       => Try(NBTInt(stream.readInt()).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagLong      => Try(NBTLong(stream.readLong()).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagFloat     => Try(NBTFloat(stream.readFloat()).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagDouble    => Try(NBTDouble(stream.readDouble()).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagByteArray => readByteArray(stream).map(a => NBTByteArray(a).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagString    => readString(stream).map(s => NBTString(s).asInstanceOf[NBTTag.Aux[A]])
      case unsafe.TagList       => readList(stream).asInstanceOf[Try[NBTTag.Aux[A]]]
      case NBTType.TagCompound  => readCompound(stream, NBTCompound()).asInstanceOf[Try[NBTTag.Aux[A]]]
      case NBTType.TagIntArray  => readIntArray(stream).map(a => NBTIntArray(a).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagLongArray => readLongArray(stream).map(a => NBTLongArray(a).asInstanceOf[NBTTag.Aux[A]])
      case NBTType.TagEnd       => Failure(new IOException("Unexpected end tag"))
      case _                    => Failure(new IOException("Unexpected tag type"))
    }
}
