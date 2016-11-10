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

sealed trait NBTType extends NBTView {
	type NBT <: NBTTag.Aux[Repr]
	def id: Byte
	override def unapply(arg: NBT): Option[Repr] = Some(arg.value)
}

object NBTType {

	type Aux[Repr0, NBT0 <: NBTTag.Aux[Repr0]] = NBTType { type Repr = Repr0; type NBT = NBT0 }
	type Repr[Repr] = Aux[Repr, NBTTag.Aux[Repr]]

	def apply[Repr, NBT <: NBTTag.Aux[Repr]](implicit nbtType: NBTType.Aux[Repr, NBT]): NBTType.Aux[Repr, NBT] = nbtType
}

trait NBTTypeInstances {

	//Official names for them
	case object TAG_END extends NBTType {
		override type Repr = Nothing
		override type NBT = NBTTag.Aux[Nothing]
		override def id: Byte = 0
		override def apply(v: Nothing): NBTTag.Aux[Nothing] = throw new IllegalStateException("Tried to construct nothing tag")
		override def unapply(arg: NBT): Option[Repr] = throw new IllegalStateException("Tried to deconstruct nothing tag")
	}

	implicit case object TAG_BYTE extends NBTType {
		override type Repr = Byte
		override type NBT = NBTByte#Self
		override def id: Byte = 1
		override def apply(v: Repr): NBT = NBTByte(v)
	}

	implicit case object TAG_SHORT extends NBTType {
		override type Repr = Short
		override type NBT = NBTShort#Self
		override def id: Byte = 2
		override def apply(v: Repr): NBT = NBTShort(v)
	}

	implicit case object TAG_INT extends NBTType {
		override type Repr = Int
		override type NBT = NBTInt#Self
		override def id: Byte = 3
		override def apply(v: Repr): NBT = NBTInt(v)
	}

	implicit case object TAG_LONG extends NBTType {
		override type Repr = Long
		override type NBT = NBTLong#Self
		override def id: Byte = 4
		override def apply(v: Repr): NBT = NBTLong(v)
	}

	implicit case object TAG_FLOAT extends NBTType {
		override type Repr = Float
		override type NBT = NBTFloat#Self
		override def id: Byte = 5
		override def apply(v: Repr): NBT = NBTFloat(v)
	}

	implicit case object TAG_DOUBLE extends NBTType {
		override type Repr = Double
		override type NBT = NBTDouble#Self
		override def id: Byte = 6
		override def apply(v: Repr): NBT = NBTDouble(v)
	}

	implicit case object TAG_BYTE_ARRAY extends NBTType {
		override type Repr = IndexedSeq[Byte]
		override type NBT = NBTByteArray#Self
		override def id: Byte = 7
		override def apply(v: Repr): NBT = NBTByteArray(v)
	}

	implicit case object TAG_STRING extends NBTType {
		override type Repr = String
		override type NBT = NBTString#Self
		override def id: Byte = 8
		override def apply(v: Repr): NBT = NBTString(v)
	}

	implicit case object TAG_COMPOUND extends NBTType {
		override type Repr = Map[String, NBTTag]
		override type NBT = NBTCompound#Self
		override def id: Byte = 10
		override def apply(v: Repr): NBT = NBTCompound(v)
	}

	implicit case object TAG_INT_ARRAY extends NBTType {
		override type Repr = IndexedSeq[Int]
		override type NBT = NBTIntArray#Self
		override def id: Byte = 11
		override def apply(v: Repr): NBT = NBTIntArray(v)
	}



	//We allow creating new list types for type sake
	abstract class NBTListType extends NBTType {
		type ElementRepr
		type ElementNBT <: NBTTag.Aux[ElementRepr]
		override type NBT = NBTList[ElementRepr, ElementNBT]
		override type Repr = Seq[ElementNBT]
		override final def id: Byte = 11

		override def apply(v: Repr): NBT = new NBTList[ElementRepr, ElementNBT](v)(this, elementType)
		def elementType: NBTType.Aux[ElementRepr, ElementNBT]
	}

	//A raw list with no checks. If used wrong, this WILL cause problems
	case object TAG_LIST extends NBTListType {
		override type ElementRepr = Nothing
		override type ElementNBT = NBTTag.Aux[Nothing]
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_END
	}

	implicit case object ListByte extends NBTListType {
		override type ElementRepr = Byte
		override type ElementNBT = NBTByte#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_BYTE
	}

	implicit case object ListShort extends NBTListType {
		override type ElementRepr = Short
		override type ElementNBT = NBTShort#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_SHORT
	}

	implicit case object ListInt extends NBTListType {
		override type ElementRepr = Int
		override type ElementNBT = NBTInt#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_INT
	}

	implicit case object ListLong extends NBTListType {
		override type ElementRepr = Long
		override type ElementNBT = NBTLong#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_LONG
	}

	implicit case object ListFloat extends NBTListType {
		override type ElementRepr = Float
		override type ElementNBT = NBTFloat#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_FLOAT
	}

	implicit case object ListDouble extends NBTListType {
		override type ElementRepr = Double
		override type ElementNBT = NBTDouble#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_DOUBLE
	}

	implicit case object ListByteArray extends NBTListType {
		override type ElementRepr = IndexedSeq[Byte]
		override type ElementNBT = NBTByteArray#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_BYTE_ARRAY
	}

	implicit case object ListString extends NBTListType {
		override type ElementRepr = String
		override type ElementNBT = NBTString#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_STRING
	}

	implicit case object ListCompound extends NBTListType {
		override type ElementRepr = Map[String, NBTTag]
		override type ElementNBT = NBTCompound#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_COMPOUND
	}

	implicit case object ListIntArray extends NBTListType {
		override type ElementRepr = IndexedSeq[Int]
		override type ElementNBT = NBTIntArray#Self
		override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.TAG_INT_ARRAY
	}

	def idToType(i: Int): Option[NBTType] = i match {
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
