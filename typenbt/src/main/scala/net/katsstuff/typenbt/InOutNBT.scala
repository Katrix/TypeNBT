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
package net.katsstuff.typenbt

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object InOutNBT {

  private final val UTF8 = StandardCharsets.UTF_8

  /**
		* Writes an [[net.katsstuff.typenbt.NBTCompound]] to an [[java.io.OutputStream]]
		*
		* @param stream The stream to write to
		* @param compound The tag to write out
		* @param rootName The name of the root of the NBT. Usually blank.
		* @param gzip If the stream should be Gziped or not
		*/
  def writeTo(stream: OutputStream, compound: NBTCompound, rootName: String = "", gzip: Boolean): Try[Unit] = {
    val newStream = new DataOutputStream(
      if (gzip) new BufferedOutputStream(new GZIPOutputStream(stream))
      else stream
    )
    try {
      for {
        _   <- writeType(newStream, compound.nbtType)
        _   <- writeString(newStream, rootName)
        res <- writeCompound(newStream, compound)
      } yield res
    } finally {
      newStream.close()
    }
  }

  /**
		* Reads an [[net.katsstuff.typenbt.NBTCompound]] from an [[java.io.InputStream]]
		*
		* @param stream The stream to read from
		* @param gzip If the [[net.katsstuff.typenbt.NBTCompound]] to read from the stream is GZiped or not
		* @return A tuple compromising of the [[net.katsstuff.typenbt.NBTCompound]] read, as well as the root name
		*/
  def readFrom(stream: InputStream, gzip: Boolean): Try[(String, NBTCompound)] = {
    val newStream = new DataInputStream(
      if (gzip) new BufferedInputStream(new GZIPInputStream(stream))
      else stream
    )
    try {
      readType(newStream) match {
        case Success(nbtType) if nbtType == NBTView.TagCompound =>
          for {
            name <- readString(newStream)
            tag  <- readCompound(newStream, NBTCompound())
          } yield (name, tag)
        case Success(_)                     => Failure(throw new IOException("Wrong starting type for NBT"))
        case f: Failure[Nothing @unchecked] => f
      }
    } finally {
      newStream.close()
    }
  }

  private def writeEndTag(stream: DataOutputStream): Try[Unit] = Try(stream.writeByte(0))

  private def writeCompound(stream: DataOutputStream, nbt: NBTCompound): Try[Unit] = {

    @tailrec
    def inner(remaining: Map[String, NBTTag], prev: Try[Unit]): Try[Unit] =
      if (remaining.isEmpty || prev.isFailure) prev
      else {
        val (name, tag) = remaining.head
        val next = for {
          _   <- writeType(stream, tag.nbtType)
          _   <- writeString(stream, name)
          res <- writeTag(stream, tag)
        } yield res

        inner(remaining.tail, next)
      }

    inner(nbt.value, Success(Unit)).flatMap(_ => writeEndTag(stream))
  }

  private def writeString(stream: DataOutputStream, string: String): Try[Unit] =
    for {
      _   <- Try(stream.writeShort(string.length))
      res <- Try(stream.write(string.getBytes(UTF8)))
    } yield res

  private def writeList(stream: DataOutputStream, list: NBTList[_, _ <: NBTTag]): Try[Unit] = {
    val ret = for {
      _   <- writeType(stream, list.nbtType.elementType)
      res <- Try(stream.writeInt(list.value.size))
    } yield res

    list.value.foldLeft(ret) {
      case (Success(_), tag)   => writeTag(stream, tag)
      case (f @ Failure(_), _) => f
    }
  }

  private def writeByteArray(stream: DataOutputStream, array: Array[Byte]): Try[Unit] =
    for {
      _   <- Try(stream.writeInt(array.length))
      res <- Try(stream.write(array))
    } yield res

  private def writeIntArray(stream: DataOutputStream, array: Array[Int]): Try[Unit] =
    array.foldLeft(Try(stream.writeInt(array.length))) {
      case (Success(_), int)   => Try(stream.writeInt(int))
      case (f @ Failure(_), _) => f
    }

  private def writeLongArray(stream: DataOutputStream, array: Array[Long]): Try[Unit] = {
    array.foldLeft(Try(stream.writeInt(array.length))) {
      case (Success(_), long)  => Try(stream.writeLong(long))
      case (f @ Failure(_), _) => f
    }
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
  private def readCompound(stream: DataInputStream, compound: NBTCompound): Try[NBTCompound] =
    //We match to be tail recursive
    readType(stream) match {
      case Success(nbtType) if nbtType == NBTView.TagEnd => Success(compound)
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

  private def readString(stream: DataInputStream): Try[String] =
    for {
      length <- Try(stream.readShort())
      characters = new Array[Byte](length)
      _ <- Try(stream.readFully(characters))
    } yield new String(characters, UTF8)

  private type AnyTag = NBTTag.Aux[Any]

  private def readList(stream: DataInputStream): Try[NBTTag] =
    for {
      nbtType <- readType(stream)
      listType = new NBTListType(nbtType.asInstanceOf[NBTType[Any, AnyTag]])
      length <- Try(stream.readInt())
      res <- (0 until length).foldLeft(Try(NBTList()(listType))) {
        case (Success(list), _)  => readTag(stream, nbtType).map(read => list :+ read.asInstanceOf[AnyTag])
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
      _ <- (0 until length).foldLeft[Try[Unit]](Success(Unit)) {
        case (Success(_), i)     => Try(array(i) = stream.readInt())
        case (f @ Failure(_), _) => f
      }
    } yield array

  private def readLongArray(stream: DataInputStream): Try[Array[Long]] =
    for {
      length <- Try(stream.readInt())
      array = new Array[Long](length)
      _ <- (0 until length).foldLeft[Try[Unit]](Success(Unit)) {
        case (Success(_), i)     => Try(array(i) = stream.readLong())
        case (f @ Failure(_), _) => f
      }
    } yield array

  private def readType(stream: DataInputStream): Try[NBTType[Any, _ <: AnyTag]] = Try {
    val byte = stream.readByte()
    NBTType
      .idToType(byte)
      .getOrElse(throw new IOException(s"Read type $byte on NBT is not valid"))
      .asInstanceOf[NBTType[Any, _ <: AnyTag]]
  }

  private def readTag[Repr](stream: DataInputStream, nbtType: NBTType[Repr, _ <: NBTTag.Aux[Repr]]): Try[NBTTag] =
    (nbtType: @unchecked) match {
      case NBTView.TagByte      => Try(NBTByte(stream.readByte()))
      case NBTView.TagShort     => Try(NBTShort(stream.readShort()))
      case NBTView.TagInt       => Try(NBTInt(stream.readInt()))
      case NBTView.TagLong      => Try(NBTLong(stream.readLong()))
      case NBTView.TagFloat     => Try(NBTFloat(stream.readFloat()))
      case NBTView.TagDouble    => Try(NBTDouble(stream.readDouble()))
      case NBTView.TagByteArray => readByteArray(stream).map(a => NBTByteArray(a))
      case NBTView.TagString    => readString(stream).map(s => NBTString(s))
      case NBTView.TagList      => readList(stream)
      case NBTView.TagCompound  => readCompound(stream, NBTCompound())
      case NBTView.TagIntArray  => readIntArray(stream).map(a => NBTIntArray(a))
      case NBTView.TagLongArray => readLongArray(stream).map(a => NBTLongArray(a))
      case NBTView.TagEnd       => throw new IOException("Unexpected end tag")
    }
}
