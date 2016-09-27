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

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox

trait NBTView[Repr, NBT <: NBTTag[_]] {
	implicit def apply(v: Repr): NBT
	implicit def unapply(arg: NBT): Option[Repr]
}

object NBTView extends NBTViewInstances with NBTTypeInstances {

	def apply[Repr, NBT <: NBTTag[_]](implicit view: NBTView[Repr, NBT]): NBTView[Repr, NBT] = view

	//We let this one be implicit so that it's recursive on case classes
	implicit def apply[A]: NBTView[A, NBTCompound] = macro NBTCaseView.createView[A]

	implicit class Ops[Repr](val v: Repr) extends AnyVal {
		def nbt[NBT <: NBTTag[_]](implicit nbtType: NBTView[Repr, NBT]): NBT = nbtType(v)
		def nbt[NBTRepr, NBT <: NBTTag[_]](implicit nbtView: NBTView[NBTRepr, NBT], convert: Repr => NBTRepr): NBT = nbtView(v)
	}
}

class NBTCaseView(val c: blackbox.Context) {

	import c.universe._

	private val nbtTypeMap = Map(
		typeOf[Byte] -> typeOf[NBTByte],
		typeOf[Short] -> typeOf[NBTShort],
		typeOf[Int] -> typeOf[NBTInt],
		typeOf[Long] -> typeOf[NBTLong],
		typeOf[Float] -> typeOf[NBTFloat],
		typeOf[Double] -> typeOf[NBTDouble],
		typeOf[IndexedSeq[Byte]] -> typeOf[NBTByteArray],
		typeOf[String] -> typeOf[NBTString],
		//typeOf[Seq] -> typeOf[NBTList],
		typeOf[Map[String, _]] -> typeOf[NBTCompound],
		typeOf[IndexedSeq[Int]] -> typeOf[NBTIntArray]
	)


	def createView[A: WeakTypeTag]: Expr[NBTView[A, NBTCompound]] = {
		val tpe = weakTypeOf[A]
		val companion = tpe.typeSymbol.companion

		if(nbtTypeMap.values.exists(t => tpe =:= t)) {
			c.abort(c.enclosingPosition, s"$tpe is an NBT type, you can't create a new view for that")
		}

		val fields = tpe.decls.collectFirst {
			case m: MethodSymbol if m.isPrimaryConstructor => m
		}.get.paramLists.head

		val fieldsToNbt = fields.map { field =>
			val fieldType = field.typeSignature
			val optNbt = nbtTypeMap.get(fieldType)
			field -> optNbt.getOrElse(typeOf[NBTCompound])
		}.toMap

		val (toCompound, fromCompound) = fields.map { field =>
			val nbt = fieldsToNbt(field)
			val name = field.asTerm.name
			val typeSignature = field.typeSignature
			val key = name.decodedName.toString

			(q"$key -> io.github.katrix.typenbt.nbt.NBTView[$typeSignature, $nbt].apply(v.$name)",
				q"nbt.getValue[$nbt, $typeSignature]($key)")
		}.unzip

		val res = c.Expr[NBTView[A, NBTCompound]] {
			q"""
					new io.github.katrix.typenbt.nbt.NBTView[$tpe, io.github.katrix.typenbt.nbt.NBTCompound] {
				 		override implicit def apply(v: $tpe): io.github.katrix.typenbt.nbt.NBTCompound = io.github.katrix.typenbt.nbt.NBTCompound(Map(..$toCompound))
						override implicit def unapply(nbt: io.github.katrix.typenbt.nbt.NBTCompound): Option[$tpe] = {
							val valid = !Seq(..$fromCompound).contains(None)
			 				if(valid) Some($companion(..${fromCompound.map(t => q"$t.get")})) else None
						}
					}
			 """
		}

		//c.info(c.enclosingPosition, res.toString(), force = true)

		res
	}
}

trait NBTType[Repr, NBT <: NBTTag[Repr]] extends NBTView[Repr, NBT] {
	def id: Int
	override implicit def unapply(arg: NBT): Option[Repr] = Some(arg.value)
}

trait NBTViewInstances {

	implicit case object BooleanView extends NBTView[Boolean, NBTByte] {
		override implicit def apply(v: Boolean): NBTByte = NBTByte(if(v) 1 else 0)
		override implicit def unapply(arg: NBTByte): Option[Boolean] = Some(arg.value == 1)
	}

	implicit case object UUIDView extends NBTView[UUID, NBTCompound] {
		override implicit def unapply(arg: NBTCompound): Option[UUID] = ???
		override implicit def apply(v: UUID): NBTCompound = {
			NBTCompound(Map("Most" -> NBTLong(v.getMostSignificantBits), "Least" -> NBTLong(v.getLeastSignificantBits)))
		}
	}
}

trait NBTTypeInstances {

	//We allow creating new list types for type sake
	class ListType[ListRepr, A <: NBTTag[ListRepr]](implicit nbtListType: NBTType[ListRepr, A]) extends NBTType[Seq[A], NBTList[A, ListRepr]] {
		final val id: Int = 9
		override implicit def apply(v: Seq[A]): NBTList[A, ListRepr] = new NBTList[A, ListRepr](v)(nbtListType, this)
		override implicit def unapply(v: NBTList[A, ListRepr]): Option[Seq[A]] = Some(v.value)
	}

	//Official names for them
	case object TAG_END extends NBTType[Nothing, NBTTag[Nothing]] {
		override def id: Int = 0
		override implicit def apply(v: Nothing): NBTTag[Nothing] = throw new IllegalArgumentException("Tried to convert nothing to NBT end tag")
		override implicit def unapply(arg: NBTTag[Nothing]): Option[Nothing] = throw new IllegalArgumentException(
			"Tried to convert value to nbt to nothing")
	}

	implicit case object TAG_BYTE extends NBTType[Byte, NBTByte] {
		override def id: Int = 1
		override implicit def apply(v: Byte): NBTByte = NBTByte(v)
	}

	implicit case object TAG_SHORT extends NBTType[Short, NBTShort] {
		override def id: Int = 2
		override implicit def apply(v: Short): NBTShort = NBTShort(v)
	}

	implicit case object TAG_INT extends NBTType[Int, NBTInt] {
		override def id: Int = 3
		override implicit def apply(v: Int): NBTInt = NBTInt(v)
	}

	implicit case object TAG_LONG extends NBTType[Long, NBTLong] {
		override def id: Int = 4
		override implicit def apply(v: Long): NBTLong = NBTLong(v)
	}

	implicit case object TAG_FLOAT extends NBTType[Float, NBTFloat] {
		override def id: Int = 5
		override implicit def apply(v: Float): NBTFloat = NBTFloat(v)
	}

	implicit case object TAG_DOUBLE extends NBTType[Double, NBTDouble] {
		override def id: Int = 6
		override implicit def apply(v: Double): NBTDouble = NBTDouble(v)
	}

	implicit case object TAG_BYTE_ARRAY extends NBTType[IndexedSeq[Byte], NBTByteArray] {
		override def id: Int = 7
		override implicit def apply(v: IndexedSeq[Byte]): NBTByteArray = NBTByteArray(v)
	}

	implicit case object TAG_STRING extends NBTType[String, NBTString] {
		override def id: Int = 8
		override implicit def apply(v: String): NBTString = NBTString(v)
	}

	implicit case object TAG_COMPOUND extends NBTType[Map[String, NBTTag[_]], NBTCompound] {
		override def id: Int = 10
		override implicit def apply(v: Map[String, NBTTag[_]]): NBTCompound = NBTCompound(v)
	}

	implicit case object TAG_INT_ARRAY extends NBTType[IndexedSeq[Int], NBTIntArray] {
		override def id: Int = 11
		override implicit def apply(v: IndexedSeq[Int]): NBTIntArray = NBTIntArray(v)
	}

	implicit case object ListByte extends ListType[Byte, NBTByte]
	implicit case object ListShort extends ListType[Short, NBTShort]
	implicit case object ListInt extends ListType[Int, NBTInt]
	implicit case object ListLong extends ListType[Long, NBTLong]
	implicit case object ListFloat extends ListType[Float, NBTFloat]
	implicit case object ListDouble extends ListType[Double, NBTDouble]
	implicit case object ListByteArray extends ListType[IndexedSeq[Byte], NBTByteArray]
	implicit case object ListString extends ListType[String, NBTString]
	implicit case object ListCompound extends ListType[Map[String, NBTTag[_]], NBTCompound]
	implicit case object ListIntArray extends ListType[IndexedSeq[Int], NBTIntArray]

	//A raw list with no checks. If used wrong, this WILL cause problems
	case object TAG_LIST extends ListType[Nothing, NBTTag[Nothing]]()(TAG_END)

	def idToType(i: Int): Option[NBTType[_, _]] = i match {
		case 0 => Some(TAG_END)
		case 1 => Some(TAG_BYTE)
		case 2 => Some(TAG_SHORT)
		case 3 => Some(TAG_INT)
		case 4 => Some(TAG_LONG)
		case 5 => Some(TAG_FLOAT)
		case 6 => Some(TAG_DOUBLE)
		case 7 => Some(TAG_BYTE_ARRAY)
		case 8 => Some(TAG_STRING)
		case 9 => Some(TAG_LIST)
		case 10 => Some(TAG_COMPOUND)
		case 11 => Some(TAG_INT_ARRAY)
		case _ => None
	}
}
