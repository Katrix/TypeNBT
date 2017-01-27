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
package net.katsstuff.typenbt.nbt

/**
	* A specific type of [[NBTTag]]. Contains constructor and deconstructer, in addition to the numerical id.
	*/
sealed trait NBTType extends NBTView {
  type NBT <: NBTTag.Aux[Repr]
  def id: Byte
  override def unapply(arg: NBT): Option[Repr] = Some(arg.value)
}

object NBTType {

  type Aux[Repr0, NBT0 <: NBTTag.Aux[Repr0]] = NBTType { type Repr = Repr0; type NBT = NBT0 }
  type Obj[Repr]                             = Aux[Repr, NBTTag.Aux[Repr]]

  sealed class ExtractFromRepr[Repr] {
    def apply[NBT <: NBTTag.Aux[Repr]](implicit nbtType: NBTType.Aux[Repr, NBT]): Aux[Repr, NBT] = nbtType
  }

  def apply[Repr, NBT <: NBTTag.Aux[Repr]](implicit nbtType: NBTType.Aux[Repr, NBT]): NBTType.Aux[Repr, NBT] = nbtType
  def forRepr[Repr] = new ExtractFromRepr[Repr]

  /**
		* Convert a numerical id to a [[NBTType]]
		*/
  def idToType(i: Int): Option[NBTType] = i match {
    case 0  => Some(NBTView.TagEnd)
    case 1  => Some(NBTView.TagByte)
    case 2  => Some(NBTView.TagShort)
    case 3  => Some(NBTView.TagInt)
    case 4  => Some(NBTView.TagLong)
    case 5  => Some(NBTView.TagFloat)
    case 6  => Some(NBTView.TagDouble)
    case 7  => Some(NBTView.TagByteArray)
    case 8  => Some(NBTView.TagString)
    case 9  => Some(NBTView.TagList)
    case 10 => Some(NBTView.TagCompound)
    case 11 => Some(NBTView.TagIntArray)
    case _  => None
  }
}

trait NBTTypeInstances {

  val TagEnd       = TAG_End
  val TagByte      = TAG_Byte
  val TagShort     = TAG_Short
  val TagInt       = TAG_Int
  val TagLong      = TAG_Long
  val TagFloat     = TAG_Float
  val TagDouble    = TAG_Double
  val TagByteArray = TAG_Byte_Array
  val TagString    = TAG_String
  val TagCompound  = TAG_Compound
  val TagIntArray  = TAG_Int_Array
  val TagList      = TAG_List

  case object AnyTag extends NBTType {
    override type Repr = Any
    override type NBT  = NBTTag.Aux[Any]
    override def id: Byte = throw new IllegalStateException("Tried to get ID for any tag")
    override def apply(v: Repr): NBT = throw new IllegalStateException("Tried to construct any tag")
  }

  //Official names for them
  case object TAG_End extends NBTType {
    override type Repr = Nothing
    override type NBT  = NBTEnd
    override def id: Byte = 0
    override def apply(v:     Nothing): NBT          = throw new IllegalStateException("Tried to construct end tag")
    override def unapply(arg: NBT):     Option[Repr] = throw new IllegalStateException("Tried to deconstruct end tag")
  }

  implicit case object TAG_Byte extends NBTType {
    override type Repr = Byte
    override type NBT  = NBTByte#Self
    override def id: Byte = 1
    override def apply(v: Repr): NBT = NBTByte(v)
  }

  implicit case object TAG_Short extends NBTType {
    override type Repr = Short
    override type NBT  = NBTShort#Self
    override def id: Byte = 2
    override def apply(v: Repr): NBT = NBTShort(v)
  }

  implicit case object TAG_Int extends NBTType {
    override type Repr = Int
    override type NBT  = NBTInt#Self
    override def id: Byte = 3
    override def apply(v: Repr): NBT = NBTInt(v)
  }

  implicit case object TAG_Long extends NBTType {
    override type Repr = Long
    override type NBT  = NBTLong#Self
    override def id: Byte = 4
    override def apply(v: Repr): NBT = NBTLong(v)
  }

  implicit case object TAG_Float extends NBTType {
    override type Repr = Float
    override type NBT  = NBTFloat#Self
    override def id: Byte = 5
    override def apply(v: Repr): NBT = NBTFloat(v)
  }

  implicit case object TAG_Double extends NBTType {
    override type Repr = Double
    override type NBT  = NBTDouble#Self
    override def id: Byte = 6
    override def apply(v: Repr): NBT = NBTDouble(v)
  }

  implicit case object TAG_Byte_Array extends NBTType {
    override type Repr = IndexedSeq[Byte]
    override type NBT  = NBTByteArray#Self
    override def id: Byte = 7
    override def apply(v: Repr): NBT = NBTByteArray(v)
  }

  implicit case object TAG_String extends NBTType {
    override type Repr = String
    override type NBT  = NBTString#Self
    override def id: Byte = 8
    override def apply(v: Repr): NBT = NBTString(v)
  }

  implicit case object TAG_Compound extends NBTType {
    override type Repr = Map[String, NBTTag]
    override type NBT  = NBTCompound#Self
    override def id: Byte = 10
    override def apply(v: Repr): NBT = NBTCompound(v)
  }

  implicit case object TAG_Int_Array extends NBTType {
    override type Repr = IndexedSeq[Int]
    override type NBT  = NBTIntArray#Self
    override def id: Byte = 11
    override def apply(v: Repr): NBT = NBTIntArray(v)
  }

  //We allow creating new list types for type sake
  sealed abstract class NBTListType extends NBTType {
    type ElementRepr
    type ElementNBT <: NBTTag.Aux[ElementRepr]
    override type NBT  = NBTList[ElementRepr, ElementNBT]
    override type Repr = Seq[ElementNBT]
    override final def id: Byte = 11

    override def apply(v: Repr): NBT = new NBTList[ElementRepr, ElementNBT](v)(this, elementType)
    def elementType: NBTType.Aux[ElementRepr, ElementNBT]
  }

  //A raw list with no checks. If used wrong, this WILL cause problems
  case object TAG_List extends NBTListType {
    override type ElementRepr = Any
    override type ElementNBT  = NBTTag.Aux[Any]
    override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = NBTView.AnyTag
  }

  implicit def listType[ElementRepr0, ElementNBT0 <: NBTTag.Aux[ElementRepr0]](implicit elementType0: NBTType.Aux[ElementRepr0, ElementNBT0]) =
    new NBTListType {
      override type ElementRepr = ElementRepr0
      override type ElementNBT  = ElementNBT0
      override def elementType: NBTType.Aux[ElementRepr, ElementNBT] = elementType0
    }
}
