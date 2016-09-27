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
package io.github.katrix.typenbt.nbt

import java.util.UUID

import scala.annotation.tailrec
import scala.reflect.ClassTag

sealed trait NBTTag[Repr] {
	type Self <: NBTTag[Repr]
	implicit val nbtType: NBTType[Repr, Self]
	def value: Repr
}

final case class NBTByte(value: Byte) extends NBTTag[Byte] {
	override type Self = NBTByte
	override implicit val nbtType: NBTType[Byte, Self] = NBTView.TAG_BYTE
}
final case class NBTShort(value: Short) extends NBTTag[Short] {
	override type Self = NBTShort
	override implicit val nbtType: NBTType[Short, Self] = NBTView.TAG_SHORT
}
final case class NBTInt(value: Int) extends NBTTag[Int] {
	override type Self = NBTInt
	override implicit val nbtType: NBTType[Int, Self] = NBTView.TAG_INT
}
final case class NBTLong(value: Long) extends NBTTag[Long] {
	override type Self = NBTLong
	override implicit val nbtType: NBTType[Long, Self] = NBTView.TAG_LONG
}
final case class NBTFloat(value: Float) extends NBTTag[Float] {
	override type Self = NBTFloat
	override implicit val nbtType: NBTType[Float, Self] = NBTView.TAG_FLOAT
}
final case class NBTDouble(value: Double) extends NBTTag[Double] {
	override type Self = NBTDouble
	override implicit val nbtType: NBTType[Double, Self] = NBTView.TAG_DOUBLE
}
final case class NBTByteArray(value: IndexedSeq[Byte]) extends NBTTag[IndexedSeq[Byte]] {
	override type Self = NBTByteArray
	override implicit val nbtType: NBTType[IndexedSeq[Byte], Self] = NBTView.TAG_BYTE_ARRAY
}
final case class NBTString(value: String) extends NBTTag[String] {
	override type Self = NBTString
	override implicit val nbtType: NBTType[String, Self] = NBTView.TAG_STRING
}

final case class NBTList[A <: NBTTag[Repr], Repr](value: Seq[A] = Seq())
	(implicit val nbtListType: NBTType[Repr, A], val nbtType: NBTType[Seq[A], NBTList[A, Repr]]) extends NBTTag[Seq[A]] {
	override type Self = NBTList[A, Repr]

	def apply(i: Int): A = value(i)

	/**
		* Creates a new NBTList with this element prepended
		*/
	def +:(value: A): NBTList[A, Repr] = NBTList[A, Repr](value +: this.value)

	/**
		* Creates a new NBTList with this element appended
		*/
	def :+(value: A): NBTList[A, Repr] = NBTList[A, Repr](this.value :+ value)
	def ++(values: A*): NBTList[A, Repr] = NBTList[A, Repr](this.value ++ values)

	/**
		* The index of the specific element.
		*/
	def indexOf(obj: A): Int = value.indexOf(obj)

	/**
		* The size of this list.
		*/
	def size: Int = value.size

	/**
		* If this list contains no elements
		*/
	def isEmpty: Boolean = value.isEmpty
}

final case class NBTCompound(value: Map[String, NBTTag[_]] = Map()) extends NBTTag[Map[String, NBTTag[_]]] {
	override type Self = NBTCompound
	override implicit val nbtType: NBTType[Map[String, NBTTag[_]], Self] = NBTView.TAG_COMPOUND

	/**
		* The size of this compound.
		*/
	def size: Int = value.size

	def +(tuple: (String, NBTTag[_])): NBTCompound = NBTCompound(value + tuple)

	def update(key: String, tag: NBTTag[_]): NBTCompound = NBTCompound(value.updated(key, tag))

	/**
		* Associates a specific tag to a specific key.
		*
		* @param key The key to bind to.
		* @param tag The tag to set.
		* @return An [[Option]] with the previous value of the used key, or None if the key was not already used.
		*/
	def set(key: String, tag: NBTTag[_]): NBTCompound = update(key, tag)

	/**
		* Creates a NBTTag from the types passed in, and adds it to the compound.
		*
		* @param key The key to bind to.
		* @param value The value top set
		* @param view The converter to convert the value to a NBTTag
		* @tparam NBT The tag to convert to
		* @tparam Repr The type to convert from
		*/
	def setValue[NBT <: NBTTag[_], Repr](key: String, value: Repr)(implicit view: NBTView[Repr, NBT]): NBTCompound = {
		set(key, view.apply(value))
	}

	/**
		* Creates two [[NBTLong]] tags from the UUID and sets the tags.
		*
		* This method differs in behavior from the [[NBTView.UUIDView]] found in the views.
		* If you want compatibility with vanilla, use this.
		*
		* The key of the two tags are key + "Most" for the most significant bits,
		* and key + "Least" for the least significant bits.
		*
		* @return The same things goes for this as for [[set]], only here you have a [[Seq]] instead of an [[Option]]
		*/
	def setUUID(key: String, value: UUID)(implicit longType: NBTView[Long, NBTLong]): NBTCompound = {
		val ret1 = setValue[NBTLong, Long](s"${key}Most", value.getMostSignificantBits)
		val ret2 = ret1.setValue[NBTLong, Long](s"${key}Least", value.getLeastSignificantBits)

		ret2
	}

	/**
		* Tries to get a value in this [[NBTCompound]], or
		* throws an NoSuchElementException if no value is found.
		*/
	def apply(key: String): NBTTag[_] = value(key)

	/**
		* Get a value from this compound
		*/
	def get(key: String): Option[NBTTag[_]] = value.get(key)

	//Is including the ClassTag enough to make sure this is safe?
	def getValue[NBT <: NBTTag[_] : ClassTag, Repr](key: String)(implicit view: NBTView[Repr, NBT]): Option[Repr] = {
		get(key).collect {
			case view(nbt) => nbt
		}
	}

	/**
		* Tries to get an [[UUID]] created with [[setUUID]].
		*/
	def getUUID(key: String)(implicit view: NBTView[Long, NBTLong]): Option[UUID] = {
		get(s"${key}Most")
			.collect { case view(most) => get(s"${key}Least")
				.collect { case view(least) => new UUID(most, least) }
			}.flatten
	}

	@tailrec
	def getRecursive(keys: String*): Option[NBTTag[_]] = {
		val tail = keys.tail
		if(tail == Nil) get(keys.head)
		else getRecursive(tail: _*)
	}

	/**
		* Tries to merge this [[NBTCompound]] with another.
		* If a situation where both compounds contain some value with the same key arises,
		* the merge function is used.
		*/
	def mergeAdvanced(other: NBTCompound)(merge: ((String, NBTTag[_]), (String, NBTTag[_])) => (String, NBTTag[_])): NBTCompound = {
		val conflictKeys = value.keySet.intersect(other.value.keySet)

		def mergePot(first: NBTTag[_], second: NBTTag[_]): Option[NBTTag[_]] = {
			first match {
				case thisCompound: NBTCompound => second match {
					case thatCompound: NBTCompound =>
						Some(thisCompound.mergeAdvanced(thatCompound)(merge))
					case _ => None
				}
				case _ => None
			}
		}

		def handleConflict(conflicted: (String, NBTTag[_]), others: Seq[(String, NBTTag[_])]): ((String, NBTTag[_]), Seq[(String, NBTTag[_])]) = {
			val otherKV = others.find(kv => kv._1 == conflicted._1)
				.get //Get is completely safe here as we already know that both sequences contains the value
			val filteredOthers = others.filter(kv => kv != otherKV)

			mergePot(conflicted._2, otherKV._2) match {
				case Some(merged) => ((otherKV._1, merged), filteredOthers)
				case None => (merge(conflicted, otherKV), filteredOthers)
			}
		}

		@tailrec
		def inner(thisRest: Seq[(String, NBTTag[_])], thatRest: Seq[(String, NBTTag[_])], acc: Map[String, NBTTag[_]]): Map[String, NBTTag[_]] = {
			if(thisRest.isEmpty) acc ++ thatRest
			else if(thatRest.isEmpty) acc ++ thisRest
			else {
				val thisHead@(thisName, _) = thisRest.head
				val thatHead@(thatName, _) = thatRest.head

				if(conflictKeys.contains(thisName)) {
					val (merged, newThat) = handleConflict(thisHead, thatRest)
					inner(thisRest.tail, newThat, acc + merged)
				}
				else if(conflictKeys.contains(thatName)) {
					val (merged, newThis) = handleConflict(thatHead, thisRest)
					inner(thisRest.tail, newThis, acc + merged)
				}
				else inner(thisRest.tail, thatRest.tail, acc + thisHead + thatHead)
			}
		}

		NBTCompound(inner(value.toSeq, other.value.toSeq, Map()))
	}

	def merge(other: NBTCompound): NBTCompound = mergeAdvanced(other)((first, second) => second)

	def hasKey(key: String): Boolean = value.contains(key)
}

final case class NBTIntArray(value: IndexedSeq[Int]) extends NBTTag[IndexedSeq[Int]] {
	override type Self = NBTIntArray
	override implicit val nbtType: NBTType[IndexedSeq[Int], Self] = NBTView.TAG_INT_ARRAY
}