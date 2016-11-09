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

import shapeless.{TypeCase, _}
import shapeless.labelled.{FieldType, field}

trait NBTView {
	type Repr
	type NBT <: NBTTag[_]
	def apply(v: Repr): NBT
	def unapply(arg: NBT): Option[Repr]
}
object NBTView extends NBTViewInstances {

	def apply[Repr, NBT <: NBTTag[_]](implicit from: Lazy[NBTView.Aux[Repr, NBT]]): NBTView.Aux[Repr, NBT] = from.value

	type Aux[Repr0, NBT0 <: NBTTag[_]] = NBTView{ type Repr = Repr0; type NBT = NBT0 }

	type Compound[Repr] = NBTView.Aux[Repr, NBTCompound]

	implicit class ReprOps[Repr](val repr: Repr) extends AnyVal {
		def nbt[NBT <: NBTTag[_]](implicit view: NBTView.Aux[Repr, NBT]): NBT = view(repr)
	}

	implicit object EmptyProduct extends NBTView {
		override type Repr = HNil
		override type NBT = NBTCompound
		override def apply(v: HNil): NBTCompound = NBTCompound()
		override def unapply(arg: NBTCompound): Option[HNil] = Some(HNil)
	}
	implicit object EmptyCoproduct extends NBTView {
		override type Repr = CNil
		override type NBT = NBTCompound
		override def apply(v: CNil): NBTCompound = throw new IllegalStateException
		override def unapply(arg: NBTCompound): Option[CNil] = throw new IllegalStateException
	}

	implicit def product[Name <: Symbol, Head, Tail <: HList, HeadNBT <: NBTTag[HeadNBTRepr], HeadNBTRepr](
			implicit
			name: Witness.Aux[Name],
			vh: Lazy[NBTView.Aux[Head, HeadNBT]],
			vt: Lazy[NBTView.Compound[Tail]],
			tpe: TypeCase[HeadNBT]
	): NBTView = new NBTView {
		override type Repr = FieldType[Name, Head] :: Tail
		override type NBT = NBTCompound
		override def apply(v: FieldType[Name, Head] :: Tail): NBTCompound = vt.value(v.tail).set(name.value.name, vh.value(v.head))
		override def unapply(arg: NBTCompound): Option[FieldType[Name, Head] :: Tail] = {
			val head = arg.get(name.value.name).flatMap {
				case tpe(h) => vh.value.unapply(h)
				case _ => None
			}
			val tail = vt.value.unapply(arg)

			head.flatMap(h => tail.map(t => field[Name](h) :: t))
		}
	}

	implicit def coproduct[Name <: Symbol, Left, Right <: Coproduct, LeftNBT <: NBTTag[LeftNBTRepr], LeftNBTRepr](
			implicit
			name: Witness.Aux[Name],
			vl: Lazy[NBTView.Aux[Left, LeftNBT]],
			vr: Lazy[NBTView.Compound[Right]],
			tpe: TypeCase[LeftNBT]
	): NBTView = new NBTView {
		override type Repr = FieldType[Name, Left] :+: Right
		override type NBT = NBTCompound
		override def apply(v: FieldType[Name, Left] :+: Right): NBTCompound = v match {
			case Inl(l) => NBTCompound(Map(name.value.name -> vl.value(l)))
			case Inr(r) => vr.value(r)
		}
		override def unapply(arg: NBTCompound): Option[FieldType[Name, Left] :+: Right] = {
			arg.get(name.value.name) match {
				case Some(tpe(tag)) => vl.value.unapply(tag).map(l => Inl(field[Name](l)))
				case _ => vr.value.unapply(arg).map(Inr(_))
			}
		}
	}

	implicit def caseToView[A, HList](
			implicit
			gen: LabelledGeneric.Aux[A, HList],
			ser: Lazy[NBTView.Compound[HList]]
	): NBTView = new NBTView {
		override type Repr = A
		override type NBT = NBTCompound
		override def apply(v: A): NBTCompound = ser.value(gen.to(v))
		override def unapply(arg: NBTCompound): Option[A] = ser.value.unapply(arg).map(gen.from)
	}
}

trait NBTViewInstances {

	sealed trait PrimitiveNBTViews extends NBTView {
		override type NBT <: NBTTag[Repr]
		override def unapply(arg: NBT): Option[Repr] = Some(arg.value)
	}

	//DANGER, Don't use this in normal code. Just used for end tag so far
	case object NothingView extends PrimitiveNBTViews {
		override type Repr = Nothing
		override type NBT = NBTTag[Nothing]
		override def apply(v: Nothing): NBTTag[Nothing] = throw new IllegalStateException("Tried to construct nothing tag")
		override def unapply(arg: NBT): Option[Repr] = throw new IllegalStateException("Tried to deconstruct nothing tag")
	}

	implicit case object ByteView extends PrimitiveNBTViews {
		override type Repr = Byte
		override type NBT = NBTByte
		override def apply(v: Repr): NBT = NBTByte(v)
	}

	implicit case object ShortView extends PrimitiveNBTViews {
		override type Repr = Short
		override type NBT = NBTShort
		override def apply(v: Repr): NBT = NBTShort(v)
	}

	implicit case object IntView extends PrimitiveNBTViews {
		override type Repr = Int
		override type NBT = NBTInt
		override def apply(v: Repr): NBT = NBTInt(v)
	}

	implicit case object LongView extends PrimitiveNBTViews {
		override type Repr = Long
		override type NBT = NBTLong
		override def apply(v: Repr): NBT = NBTLong(v)
	}

	implicit case object FloatView extends PrimitiveNBTViews {
		override type Repr = Float
		override type NBT = NBTFloat
		override def apply(v: Repr): NBT = NBTFloat(v)
	}

	implicit case object DoubleView extends PrimitiveNBTViews {
		override type Repr = Double
		override type NBT = NBTDouble
		override def apply(v: Repr): NBT = NBTDouble(v)
	}

	implicit case object IndexedSeqByteView extends PrimitiveNBTViews {
		override type Repr = IndexedSeq[Byte]
		override type NBT = NBTByteArray
		override def apply(v: Repr): NBT= NBTByteArray(v)
	}

	implicit case object StringView extends PrimitiveNBTViews {
		override type Repr = String
		override type NBT = NBTString
		override def apply(v: Repr): NBT = NBTString(v)
	}

	implicit case object CompoundView extends PrimitiveNBTViews {
		override type Repr = Map[String, NBTTag[_]]
		override type NBT = NBTCompound
		override def apply(v: Repr): NBT = NBTCompound(v)
	}

	implicit case object IndexedSeqIntView extends PrimitiveNBTViews {
		override type Repr = IndexedSeq[Int]
		override type NBT = NBTIntArray
		override def apply(v: Repr): NBT = NBTIntArray(v)
	}

	//We allow creating new list types for type sake
	abstract class ListView extends PrimitiveNBTViews {
		type ElementRepr
		type A <: NBTTag[ElementRepr]
		override type NBT = NBTList[ElementRepr, A]
		override type Repr = Seq[A]

		override def apply(v: Repr): NBT = new NBTList[ElementRepr, A](v)(listType, elementType)
		def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]]
		def elementType: NBTType.Aux[ElementRepr, A]
	}

	//Danger
	case object ListNothingView extends ListView {
		override type ElementRepr = Nothing
		override type A = NBTTag[Nothing]
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.TAG_LIST
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_END
	}

	implicit case object ListByteView extends ListView {
		override type ElementRepr = Byte
		override type A = NBTByte
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListByte
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_BYTE
	}

	implicit case object ListShortView extends ListView {
		override type ElementRepr = Short
		override type A = NBTShort
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListShort
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_SHORT
	}

	implicit case object ListIntView extends ListView {
		override type ElementRepr = Int
		override type A = NBTInt
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListInt
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_INT
	}

	implicit case object ListLongView extends ListView {
		override type ElementRepr = Long
		override type A = NBTLong
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListLong
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_LONG
	}

	implicit case object ListFloatView extends ListView {
		override type ElementRepr = Float
		override type A = NBTFloat
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListFloat
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_FLOAT
	}

	implicit case object ListDoubleView extends ListView {
		override type ElementRepr = Double
		override type A = NBTDouble
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListDouble
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_DOUBLE
	}

	implicit case object ListByteArrayView extends ListView {
		override type ElementRepr = IndexedSeq[Byte]
		override type A = NBTByteArray
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListByteArray
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_BYTE_ARRAY
	}

	implicit case object ListStringView extends ListView {
		override type ElementRepr = String
		override type A = NBTString
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListString
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_STRING
	}

	implicit case object ListCompoundView extends ListView {
		override type ElementRepr = Map[String, NBTTag[_]]
		override type A = NBTCompound
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListCompound
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_COMPOUND
	}

	implicit case object ListIntArrayView extends ListView {
		override type ElementRepr = IndexedSeq[Int]
		override type A = NBTIntArray
		override def listType: NBTType.Aux[Seq[A], NBTList[ElementRepr, A]] = NBTType.ListIntArray
		override def elementType: NBTType.Aux[ElementRepr, A] = NBTType.TAG_INT_ARRAY
	}



	implicit case object BooleanView extends NBTView {
		override type Repr = Boolean
		override type NBT = NBTByte
		override def apply(v: Boolean): NBTByte = NBTByte(if(v) 1 else 0)
		override def unapply(arg: NBTByte): Option[Boolean] = Some(arg.value == 1)
	}

	implicit case object UUIDView extends NBTView {
		override type Repr = UUID
		override type NBT = NBTCompound
		override def unapply(arg: NBTCompound): Option[UUID] = {
			arg.get("Most").flatMap {
				case NBTLong(mostSign) => arg.get("Least").flatMap {
					case NBTLong(leastSign) => Some(new UUID(mostSign, leastSign))
					case _ => None
				}
				case _ => None
			}
		}

		override def apply(v: UUID): NBTCompound = {
			NBTCompound(Map("Most" -> NBTLong(v.getMostSignificantBits), "Least" -> NBTLong(v.getLeastSignificantBits)))
		}
	}
}