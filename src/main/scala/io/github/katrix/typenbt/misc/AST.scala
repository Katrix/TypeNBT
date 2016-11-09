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

import io.github.katrix.typenbt.nbt

object AST {

	def apply(tag: nbt.NBTTag[_]): Tag[_] = tag match {
		case nbt.NBTByte(b) => NBTByte(b)
		case nbt.NBTShort(s) => NBTShort(s)
		case nbt.NBTInt(i) => NBTInt(i)
		case nbt.NBTLong(l) => NBTLong(l)
		case nbt.NBTFloat(f) => NBTFloat(f)
		case nbt.NBTDouble(d) => NBTDouble(d)
		case nbt.NBTByteArray(array) => NBTByteArray(array)
		case nbt.NBTString(s) => NBTString(s)
		case listNbt@nbt.NBTList(list) => NBTList(listNbt.nbtListType.id, list.map(apply))
		case nbt.NBTCompound(tags) => NBTCompound(tags.map { case (name, compoundTag) => NamedTag(name -> apply(compoundTag)) }.toSeq)
		case nbt.NBTIntArray(array) => NBTIntArray(array)
	}

	def unapply(tag: Tag[_]): Option[nbt.NBTTag[_]] = {
		def sequence[T](l: Seq[Option[T]]) = if(l.contains(None)) None else Some(l.flatten)

		tag match {
			case NBTByte(b) => Some(nbt.NBTByte(b))
			case NBTShort(s) => Some(nbt.NBTShort(s))
			case NBTInt(i) => Some(nbt.NBTInt(i))
			case NBTLong(l) => Some(nbt.NBTLong(l))
			case NBTFloat(f) => Some(nbt.NBTFloat(f))
			case NBTDouble(d) => Some(nbt.NBTDouble(d))
			case NBTByteArray(array) => Some(nbt.NBTByteArray(array))
			case NBTString(s) => Some(nbt.NBTString(s))
			case NBTList(id, list) =>
				val sequenced = sequence(list.map(unapply))
				//Biggest hack ever
				sequenced.flatMap(tagList => nbt.NBTType.idToType(id).map(listType =>
					nbt.NBTList[Nothing, nbt.NBTTag[Nothing]](tagList.asInstanceOf[Seq[nbt.NBTTag[Nothing]]])
						(nbt.NBTType.TAG_LIST, listType.asInstanceOf[nbt.NBTType.Aux[Nothing, nbt.NBTTag[Nothing]]])))
			case NBTCompound(tags) =>
				val converted: Seq[Option[(String, nbt.NBTTag[_])]] = tags.map {
					case NamedTag((name, AST(nbtTag))) => Some(name -> nbtTag)
					case _ => None
				}
				val sequenced = sequence(converted)
				sequenced.map(seq => nbt.NBTCompound(seq.toMap))
			case NBTIntArray(array) => Some(nbt.NBTIntArray(array))
		}
	}

	sealed trait NBTAST[A] {
		def value: A
	}

	sealed abstract class Tag[A](val id: Byte) extends NBTAST[A]

	final case class NamedTag(value: (String, Tag[_])) extends NBTAST[(String, Tag[_])]

	final case class NBTByte(value: Byte) extends Tag[Byte](Ids.Byte)
	final case class NBTShort(value: Short) extends Tag[Short](Ids.Short)
	final case class NBTInt(value: Int) extends Tag[Int](Ids.Int)
	final case class NBTLong(value: Long) extends Tag[Long](Ids.Long)
	final case class NBTFloat(value: Float) extends Tag[Float](Ids.Float)
	final case class NBTDouble(value: Double) extends Tag[Double](Ids.Double)
	final case class NBTByteArray(value: IndexedSeq[Byte]) extends Tag[IndexedSeq[Byte]](Ids.ByteArray)
	final case class NBTString(value: String) extends Tag[String](Ids.String)
	final case class NBTList(listId: Byte, value: Seq[Tag[_]]) extends Tag[Seq[Tag[_]]](Ids.List)
	final case class NBTCompound(value: Seq[NamedTag]) extends Tag[Seq[NamedTag]](Ids.Compound)
	final case class NBTIntArray(value: IndexedSeq[Int]) extends Tag[IndexedSeq[Int]](Ids.IntArray)

	object Ids {
		final val End      : Byte = 0
		final val Byte     : Byte = 1
		final val Short    : Byte = 2
		final val Int      : Byte = 3
		final val Long     : Byte = 4
		final val Float    : Byte = 5
		final val Double   : Byte = 6
		final val ByteArray: Byte = 7
		final val String   : Byte = 8
		final val List     : Byte = 9
		final val Compound : Byte = 10
		final val IntArray : Byte = 11
	}
}