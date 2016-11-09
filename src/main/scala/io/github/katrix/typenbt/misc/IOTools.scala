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
package io.github.katrix.typenbt.misc

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import io.github.katrix.typenbt.misc.AST._
import io.github.katrix.typenbt.nbt

object IOTools {

	private final val UTF8 = StandardCharsets.UTF_8

	/**
		* Writes an [[nbt.NBTCompound]] to an [[OutputStream]]
		*
		* @param stream The stream to write to
		* @param compound The tag to write out
		* @param rootName The name of the root of the NBT. Usually not seen.
		* @param gzip If the stream should be Gziped or not
		*/
	def writeTo(stream: OutputStream, compound: nbt.NBTCompound, rootName: String, gzip: Boolean): Try[Unit] = {
		val newStream = new DataOutputStream(if(gzip) new BufferedOutputStream(new GZIPOutputStream(stream)) else stream)
		//We use the AST so that we don't have to deal with the nightmare that is list types where we don't know the specific type
		val ast = AST(compound).asInstanceOf[NBTCompound]
		try {
			for {
				_ <- writeType(newStream, ast.id)
				_ <- writeString(newStream, rootName)
				res <- writeCompound(newStream, ast)
			} yield res
		}
		finally {
			newStream.close()
		}
	}

	/**
		* Reads an [[nbt.NBTCompound]] from an [[InputStream]]
		*
		* @param stream The stream to read from
		* @param gzip If the [[nbt.NBTCompound]] to read from the stream is GZiped or not
		* @return A tuple compromising of the [[nbt.NBTCompound]] read, as well as the root name
		*/
	def readFrom(stream: InputStream, gzip: Boolean): Try[(String, nbt.NBTCompound)] = {
		val newStream = new DataInputStream(if(gzip) new BufferedInputStream(new GZIPInputStream(stream)) else stream)
		val ret = try {
			readType(newStream) match {
				case Success(nbtType) if nbtType == Ids.Compound =>
					for {
						name <- readString(newStream)
						tag <- readCompound(newStream, NBTCompound(Seq()))
					} yield (name, tag)
				case Success(nbtType) => Failure(throw new IOException("Wrong starting type for NBT"))
				case f: Failure[Nothing@unchecked] => f
			}
		}
		finally {
			newStream.close()
		}

		ret.flatMap {
			case (name, AST(convertedNbt)) => Success((name, convertedNbt.asInstanceOf[nbt.NBTCompound]))
			case _ => Failure(new IllegalStateException("Could not convert read AST to NBTCompound"))
		}
	}

	private def writeEndTag(stream: DataOutputStream): Try[Unit] = Try(stream.writeByte(0))

	private def writeCompound(stream: DataOutputStream, nbt: NBTCompound): Try[Unit] = {

		@tailrec
		def inner(remaining: Seq[NamedTag], prev: Try[Unit]): Try[Unit] = {
			if(remaining == Nil || prev.isFailure) prev
			else {
				val NamedTag((name, tag)) = remaining.head
				val next = for {
					_ <- writeType(stream, tag.id)
					_ <- writeString(stream, name)
					res <- writeTag(stream, tag)
				} yield res

				inner(remaining.tail, next)
			}
		}

		inner(nbt.value, Success(Unit)).flatMap(u => writeEndTag(stream))
	}

	private def writeString(stream: DataOutputStream, string: String): Try[Unit] = for {
		_ <- Try(stream.writeShort(string.length))
		res <- Try(stream.write(string.getBytes(UTF8)))
	} yield res

	private def writeList(stream: DataOutputStream, list: NBTList): Try[Unit] = {
		val ret = for {
			_ <- writeType(stream, list.listId)
			res <- Try(stream.writeInt(list.value.size))
		} yield res

		list.value.foldLeft(ret) {
			case (Success(_), tag) => writeTag(stream, tag)
			case (f@Failure(_), _) => f
		}
	}

	private def writeByteArray(stream: DataOutputStream, array: Array[Byte]): Try[Unit] = for {
		_ <- Try(stream.writeInt(array.length))
		res <- Try(stream.write(array))
	} yield res

	private def writeIntArray(stream: DataOutputStream, array: Array[Int]): Try[Unit] = {
		array.foldLeft(Try(stream.writeInt(array.length))) {
			case (Success(_), int) => Try(stream.writeInt(int))
			case (f@Failure(_), _) => f
		}
	}

	private def writeType(stream: DataOutputStream, tagType: Byte): Try[Unit] = Try(stream.writeByte(tagType))

	private def writeTag(stream: DataOutputStream, nbt: Tag[_]): Try[Unit] = {
		nbt match {
			case NBTByte(b) => Try(stream.writeByte(b))
			case NBTShort(s) => Try(stream.writeShort(s))
			case NBTInt(i) => Try(stream.writeInt(i))
			case NBTLong(l) => Try(stream.writeLong(l))
			case NBTFloat(f) => Try(stream.writeFloat(f))
			case NBTDouble(d) => Try(stream.writeDouble(d))
			case NBTByteArray(array) => writeByteArray(stream, array.toArray)
			case NBTString(s) => writeString(stream, s)
			case list: NBTList => writeList(stream, list)
			case compound: NBTCompound => writeCompound(stream, compound)
			case NBTIntArray(intArray) => writeIntArray(stream, intArray.toArray)
		}
	}

	@tailrec
	private def readCompound(stream: DataInputStream, compound: NBTCompound): Try[NBTCompound] = {
		//We match to be tail recursive
		readType(stream) match {
			case Success(nbtType) =>
				if(nbtType == Ids.End ) Success(compound)
				else {
					readString(stream) match {
						case Success(name) => readTag(stream, nbtType) match {
							case Success(tag) => readCompound(stream, compound.copy(value = compound.value :+ NamedTag((name, tag))))
							case f: Failure[Nothing@unchecked] => f
						}
						case f: Failure[Nothing@unchecked] => f
					}
				}
			case f: Failure[Nothing@unchecked] => f
		}
	}

	private def readString(stream: DataInputStream): Try[String] = Try(stream.readShort()).flatMap(length => {
		val characters = new Array[Byte](length)
		val readBytes = Try(stream.readFully(characters))
		readBytes.map(u => new String(characters, UTF8))
	})

	private def readList(stream: DataInputStream): Try[NBTList] = {
		val ret = for {
			nbtType <- readType(stream)
			length <- Try(stream.readInt())
		} yield {

			(0 until length).foldLeft(Success(NBTList(nbtType, Seq())): Try[NBTList]) {
				case (Success(list), _) => readTag(stream, nbtType).map(read => list.copy(value = list.value :+ read))
				case (f@Failure(_), _) => f
			}
		}

		ret.flatten
	}

	private def readByteArray(stream: DataInputStream): Try[Array[Byte]] = {
		Try(stream.readInt()).flatMap(length => {
			val bytes = new Array[Byte](length)
			val readBytes = Try(stream.readFully(bytes))
			readBytes.map(u => bytes)
		})
	}

	private def readIntArray(stream: DataInputStream): Try[Array[Int]] = {
		Try(stream.readInt()).map(length => {
			val array = new Array[Int](length)
			val res = (0 until  length).foldLeft(Success(Unit): Try[Unit]){
				case (Success(_), i) => Try(array(i) = stream.readInt())
				case (f@Failure(_), _) => f
			}
			res.map(u => array)
		}).flatten
	}

	private def readType(stream: DataInputStream): Try[Byte] = Try(stream.readByte())

	private def readTag(stream: DataInputStream, nbtType: Byte): Try[Tag[_]] = nbtType match {
		case Ids.Byte => Try(NBTByte(stream.readByte()))
		case Ids.Short => Try(NBTShort(stream.readShort()))
		case Ids.Int => Try(NBTInt(stream.readInt()))
		case Ids.Long => Try(NBTLong(stream.readLong()))
		case Ids.Float => Try(NBTFloat(stream.readFloat()))
		case Ids.Double => Try(NBTDouble(stream.readDouble()))
		case Ids.ByteArray => readByteArray(stream).map(a => NBTByteArray(a))
		case Ids.String => readString(stream).map(s => NBTString(s))
		case Ids.List => readList(stream)
		case Ids.Compound => readCompound(stream, NBTCompound(Seq()))
		case Ids.IntArray => readIntArray(stream).map(a => NBTIntArray(a))
		case Ids.End => throw new IOException("Unexpected end tag")
	}
}