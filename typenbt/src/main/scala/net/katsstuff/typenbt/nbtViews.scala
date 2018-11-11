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

import java.util.UUID

import scala.language.{existentials, higherKinds}

/**
  * A typeclass responsible for serializing values into NBT.
  *
  * @tparam Repr The type it serializes.
  * @tparam NBT The resulting NBT type.
  */
//noinspection ConvertExpressionToSAM
trait NBTSerializer[-Repr, +NBT <: NBTTag] { self =>

  /**
    * Convert a value to NBT.
    */
  def to(v: Repr): NBT

  /**
    * Create a new serializer that uses this serializer as a stepping stone.
    */
  def contramap[NewRepr](f: NewRepr => Repr): NBTSerializer[NewRepr, NBT] = new NBTSerializer[NewRepr, NBT] {
    override def to(v: NewRepr): NBT = self.to(f(v))
  }

  /**
    * Maps the NBT that resulted from using this serializer.
    */
  def mapNbt[NewNBT <: NBTTag](f: NBT => NewNBT): NBTSerializer[Repr, NewNBT] = new NBTSerializer[Repr, NewNBT] {
    override def to(v: Repr): NewNBT = f(self.to(v))
  }
}
object NBTSerializer extends LowPriorityNBTSerializers {
  def apply[Repr, NBT <: NBTTag](implicit ser: NBTSerializer[Repr, NBT]): NBTSerializer[Repr, NBT] = ser

  class ReprOps[Repr](private val repr: Repr) extends AnyVal {
    def nbt[NBT <: NBTTag](implicit ser: NBTSerializer[Repr, NBT]): NBT = ser.to(repr)
  }

  class NBTOps[NBT <: NBTTag](private val nbt: NBT) extends AnyVal {
    def set[Repr](repr: Repr)(implicit ser: NBTSerializer[Repr, NBT]): NBT = ser.to(repr)
  }

  def forRepr[Repr] = new SerializerForRepr[Repr]

  class SerializerForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit ser: NBTSerializer[Repr, NBT]): NBTSerializer[Repr, NBT] = ser
  }

  val TagEnd: NBTSerializer[Nothing, NBTEnd]                                = NBTType.TAG_End
  implicit val TagByte: NBTSerializer[Byte, NBTByte]                        = NBTType.TAG_Byte
  implicit val TagShort: NBTSerializer[Short, NBTShort]                     = NBTType.TAG_Short
  implicit val TagInt: NBTSerializer[Int, NBTInt]                           = NBTType.TAG_Int
  implicit val TagLong: NBTSerializer[Long, NBTLong]                        = NBTType.TAG_Long
  implicit val TagFloat: NBTSerializer[Float, NBTFloat]                     = NBTType.TAG_Float
  implicit val TagDouble: NBTSerializer[Double, NBTDouble]                  = NBTType.TAG_Double
  implicit val TagByteArray: NBTSerializer[IndexedSeq[Byte], NBTByteArray]  = NBTType.TAG_Byte_Array
  implicit val TagString: NBTSerializer[String, NBTString]                  = NBTType.TAG_String
  implicit val TagCompound: NBTSerializer[Map[String, NBTTag], NBTCompound] = NBTType.TAG_Compound
  implicit val TagIntArray: NBTSerializer[IndexedSeq[Int], NBTIntArray]     = NBTType.TAG_Int_Array
  implicit val TagLongArray: NBTSerializer[IndexedSeq[Long], NBTLongArray]  = NBTType.TAG_Long_Array

  implicit def listType[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit elementType: NBTType[ElemRepr, ElemNBT]
  ): NBTSerializer[Seq[ElemNBT], NBTList[ElemRepr, ElemNBT]] = NBTType.listType[ElemRepr, ElemNBT]
}
//noinspection ConvertExpressionToSAM
trait LowPriorityNBTSerializers extends ExtraLowPriorityNBTSerializers {

  implicit val BooleanView: NBTSerializer[Boolean, NBTByte] = NBTBoolean
  implicit val UUIDView: NBTSerializer[UUID, NBTCompound]   = NBTUUID

  implicit def mapSer[ElemRepr, ElemNBT <: NBTTag](
      implicit ser: NBTSerializer[ElemRepr, ElemNBT]
  ): NBTSerializer[Map[String, ElemRepr], NBTCompound] = new NBTSerializer[Map[String, ElemRepr], NBTCompound] {
    override def to(v: Map[String, ElemRepr]): NBTCompound = NBTCompound(v.mapValues(ser.to))
  }
}

//noinspection ConvertExpressionToSAM
trait ExtraLowPriorityNBTSerializers {

  implicit def seqSer[ListNBTRepr, SeqRepr, ListNBT <: NBTTag.Aux[ListNBTRepr]](
      implicit ser: NBTSerializer[SeqRepr, ListNBT],
      listType: NBTListType[ListNBTRepr, ListNBT]
  ): NBTSerializer[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] =
    new NBTSerializer[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] {
      override def to(v: Seq[SeqRepr]): NBTList[ListNBTRepr, ListNBT] = NBTList[ListNBTRepr, ListNBT](v.map(ser.to))
    }
}

/**
  * A typeclass responsible for deserializing values from NBT.
  *
  * @tparam Repr The type it deserialize to.
  * @tparam NBT The original NBT type.
  */
//noinspection ConvertExpressionToSAM
trait NBTDeserializer[+Repr, -NBT <: NBTTag] { self =>

  /**
    * Convert a value from NBT.
    */
  def from(arg: NBT): Option[Repr]

  /**
    * Map the result of running this deserializer.
    */
  def map[NewRepr](f: Repr => NewRepr): NBTDeserializer[NewRepr, NBT] = new NBTDeserializer[NewRepr, NBT] {
    override def from(arg: NBT): Option[NewRepr] = self.from(arg).map(f)
  }

  /**
    * Map the result of running this deserializer using a function that can fail.
    */
  def optMap[NewRepr](f: Repr => Option[NewRepr]): NBTDeserializer[NewRepr, NBT] = new NBTDeserializer[NewRepr, NBT] {
    override def from(arg: NBT): Option[NewRepr] = self.from(arg).flatMap(f)
  }

  /**
    * Create a new deserializer that changes the NBT type and uses this
    * deserializer as a stepping stone.
    */
  def contramapNbt[NewNBT <: NBTTag](f: NewNBT => NBT): NBTDeserializer[Repr, NewNBT] =
    new NBTDeserializer[Repr, NewNBT] {
      override def from(arg: NewNBT): Option[Repr] = self.from(f(arg))
    }
}
object NBTDeserializer extends LowPriorityNBTDeserializers {
  def apply[Repr, NBT <: NBTTag](implicit deser: NBTDeserializer[Repr, NBT]): NBTDeserializer[Repr, NBT] = deser

  class NBTOps[NBT <: NBTTag](private val nbt: NBT) extends AnyVal {
    def as[Repr](implicit deser: NBTDeserializer[Repr, NBT]): Option[Repr] = deser.from(nbt)
  }

  def forRepr[Repr] = new DeserializerForRepr[Repr]

  class DeserializerForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit deser: NBTDeserializer[Repr, NBT]): NBTDeserializer[Repr, NBT] = deser
  }

  val TagEnd: NBTDeserializer[Nothing, NBTEnd]                                = NBTType.TAG_End
  implicit val TagByte: NBTDeserializer[Byte, NBTByte]                        = NBTType.TAG_Byte
  implicit val TagShort: NBTDeserializer[Short, NBTShort]                     = NBTType.TAG_Short
  implicit val TagInt: NBTDeserializer[Int, NBTInt]                           = NBTType.TAG_Int
  implicit val TagLong: NBTDeserializer[Long, NBTLong]                        = NBTType.TAG_Long
  implicit val TagFloat: NBTDeserializer[Float, NBTFloat]                     = NBTType.TAG_Float
  implicit val TagDouble: NBTDeserializer[Double, NBTDouble]                  = NBTType.TAG_Double
  implicit val TagByteArray: NBTDeserializer[IndexedSeq[Byte], NBTByteArray]  = NBTType.TAG_Byte_Array
  implicit val TagString: NBTDeserializer[String, NBTString]                  = NBTType.TAG_String
  implicit val TagCompound: NBTDeserializer[Map[String, NBTTag], NBTCompound] = NBTType.TAG_Compound
  implicit val TagIntArray: NBTDeserializer[IndexedSeq[Int], NBTIntArray]     = NBTType.TAG_Int_Array
  implicit val TagLongArray: NBTDeserializer[IndexedSeq[Long], NBTLongArray]  = NBTType.TAG_Long_Array

  implicit def listType[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit elementType: NBTType[ElemRepr, ElemNBT]
  ): NBTDeserializer[Seq[ElemNBT], NBTList[ElemRepr, ElemNBT]] = NBTType.listType[ElemRepr, ElemNBT]
}
trait LowPriorityNBTDeserializers extends ExtraLowPriorityNBTDeserializers {
  implicit val BooleanView: NBTDeserializer[Boolean, NBTByte] = NBTBoolean
  implicit val UUIDView: NBTDeserializer[UUID, NBTCompound]   = NBTUUID
}

//noinspection ConvertExpressionToSAM
trait ExtraLowPriorityNBTDeserializers {

  implicit def seqDeser[ListNBTRepr, SeqRepr, ListNBT <: NBTTag.Aux[ListNBTRepr]](
      implicit deser: NBTDeserializer[SeqRepr, ListNBT]
  ): NBTDeserializer[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] =
    new NBTDeserializer[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] {
      override def from(arg: NBTList[ListNBTRepr, ListNBT]): Option[Seq[SeqRepr]] = Some(arg.value.flatMap(deser.from))
    }
}

//noinspection ConvertExpressionToSAM
trait SafeNBTDeserializer[+Repr, -NBT <: NBTTag] extends NBTDeserializer[Repr, NBT] { self =>

  override def from(arg: NBT): Option[Repr] = Some(fromSafe(arg))

  /**
    * A safer version of [[NBTDeserializer.from]] that can't fail.
    */
  def fromSafe(arg: NBT): Repr

  override def map[NewRepr](f: Repr => NewRepr): SafeNBTDeserializer[NewRepr, NBT] =
    new SafeNBTDeserializer[NewRepr, NBT] {
      override def fromSafe(arg: NBT): NewRepr = f(self.fromSafe(arg))
    }

  override def contramapNbt[NewNBT <: NBTTag](f: NewNBT => NBT): SafeNBTDeserializer[Repr, NewNBT] =
    new SafeNBTDeserializer[Repr, NewNBT] {
      override def fromSafe(arg: NewNBT): Repr = self.fromSafe(f(arg))
    }
}
object SafeNBTDeserializer extends LowPrioritySafeNBTDeserializers {
  def apply[Repr, NBT <: NBTTag](implicit deser: SafeNBTDeserializer[Repr, NBT]): SafeNBTDeserializer[Repr, NBT] = deser

  class NBTOps[NBT <: NBTTag](private val nbt: NBT) extends AnyVal {
    def safeAs[Repr](implicit safeDeser: SafeNBTDeserializer[Repr, NBT]): Repr = safeDeser.fromSafe(nbt)
  }

  def forRepr[Repr] = new SafeDeserializerForRepr[Repr]

  class SafeDeserializerForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit safeDeser: SafeNBTDeserializer[Repr, NBT]): SafeNBTDeserializer[Repr, NBT] =
      safeDeser
  }

  val TagEnd: SafeNBTDeserializer[Nothing, NBTEnd]                                = NBTType.TAG_End
  implicit val TagByte: SafeNBTDeserializer[Byte, NBTByte]                        = NBTType.TAG_Byte
  implicit val TagShort: SafeNBTDeserializer[Short, NBTShort]                     = NBTType.TAG_Short
  implicit val TagInt: SafeNBTDeserializer[Int, NBTInt]                           = NBTType.TAG_Int
  implicit val TagLong: SafeNBTDeserializer[Long, NBTLong]                        = NBTType.TAG_Long
  implicit val TagFloat: SafeNBTDeserializer[Float, NBTFloat]                     = NBTType.TAG_Float
  implicit val TagDouble: SafeNBTDeserializer[Double, NBTDouble]                  = NBTType.TAG_Double
  implicit val TagByteArray: SafeNBTDeserializer[IndexedSeq[Byte], NBTByteArray]  = NBTType.TAG_Byte_Array
  implicit val TagString: SafeNBTDeserializer[String, NBTString]                  = NBTType.TAG_String
  implicit val TagCompound: SafeNBTDeserializer[Map[String, NBTTag], NBTCompound] = NBTType.TAG_Compound
  implicit val TagIntArray: SafeNBTDeserializer[IndexedSeq[Int], NBTIntArray]     = NBTType.TAG_Int_Array
  implicit val TagLongArray: SafeNBTDeserializer[IndexedSeq[Long], NBTLongArray]  = NBTType.TAG_Long_Array

  implicit def listType[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit elementType: NBTType[ElemRepr, ElemNBT]
  ): SafeNBTDeserializer[Seq[ElemNBT], NBTList[ElemRepr, ElemNBT]] = NBTType.listType[ElemRepr, ElemNBT]
}
trait LowPrioritySafeNBTDeserializers extends ExtraLowPrioritySafeNBTDeserializers {
  implicit val BooleanView: SafeNBTDeserializer[Boolean, NBTByte] = NBTBoolean
}
//noinspection ConvertExpressionToSAM
trait ExtraLowPrioritySafeNBTDeserializers {

  implicit def seqSafeDeser[ListNBTRepr, SeqRepr, ListNBT <: NBTTag.Aux[ListNBTRepr]](
      implicit deser: NBTDeserializer[SeqRepr, ListNBT]
  ): SafeNBTDeserializer[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] =
    new SafeNBTDeserializer[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] {
      override def fromSafe(arg: NBTList[ListNBTRepr, ListNBT]): Seq[SeqRepr] = arg.value.flatMap(deser.from)
    }
}

/**
  * A NBTView is a sort of Prism allowing you to see and modify NBT easier.
  * @tparam Repr The type it represents
  * @tparam NBT The corresponding nbt type
  */
trait NBTView[Repr, NBT <: NBTTag] extends NBTSerializer[Repr, NBT] with NBTDeserializer[Repr, NBT] { self =>

  /**
    * Modifies a nbt in value form before returning a new NBT.
    * Thew two types of NBT does not have to be the same.
    *
    * @example {{{
    *   val stringNbt: Option[NBTString] = NBTView.TagInt.modify(NBTInt(5))(_.toString)
    * }}}
    * @param nbt The NBT to modify
    * @param f The function to apply to the NBT
    * @param newView A view providing a way to get back to the world
    *                of NBTs after the modification.
    * @tparam NewRepr The new value type
    * @tparam NewNBT The new NBT type
    */
  def modify[NewRepr, NewNBT <: NBTTag](
      nbt: NBT
  )(f: Repr => NewRepr)(implicit newView: NBTView[NewRepr, NewNBT]): Option[NewNBT] =
    from(nbt).map(a => newView.to(f(a)))

  def imap[NewRepr](f: Repr => NewRepr, g: NewRepr => Repr): NBTView[NewRepr, NBT] = new NBTView[NewRepr, NBT] {
    override def to(v: NewRepr): NBT             = self.to(g(v))
    override def from(arg: NBT): Option[NewRepr] = self.from(arg).map(f)
  }

  def imapOpt[NewRepr](f: Repr => Option[NewRepr], g: NewRepr => Repr): NBTView[NewRepr, NBT] =
    new NBTView[NewRepr, NBT] {
      override def to(v: NewRepr): NBT             = self.to(g(v))
      override def from(arg: NBT): Option[NewRepr] = self.from(arg).flatMap(f)
    }

  def imapNbt[NewNBT <: NBTTag](f: NBT => NewNBT, g: NewNBT => NBT): NBTView[Repr, NewNBT] = new NBTView[Repr, NewNBT] {
    override def to(v: Repr): NewNBT             = f(self.to(v))
    override def from(arg: NewNBT): Option[Repr] = self.from(g(arg))
  }
}

object NBTView extends LowPriorityNBTViews {

  def apply[Repr, NBT <: NBTTag](implicit view: NBTView[Repr, NBT]): NBTView[Repr, NBT] = view

  def forRepr[Repr] = new ViewForRepr[Repr]

  class ViewForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit view: NBTView[Repr, NBT]): NBTView[Repr, NBT] = view
  }

  val TagEnd: NBTView[Nothing, NBTEnd]                                = NBTType.TAG_End
  implicit val TagByte: NBTView[Byte, NBTByte]                        = NBTType.TAG_Byte
  implicit val TagShort: NBTView[Short, NBTShort]                     = NBTType.TAG_Short
  implicit val TagInt: NBTView[Int, NBTInt]                           = NBTType.TAG_Int
  implicit val TagLong: NBTView[Long, NBTLong]                        = NBTType.TAG_Long
  implicit val TagFloat: NBTView[Float, NBTFloat]                     = NBTType.TAG_Float
  implicit val TagDouble: NBTView[Double, NBTDouble]                  = NBTType.TAG_Double
  implicit val TagByteArray: NBTView[IndexedSeq[Byte], NBTByteArray]  = NBTType.TAG_Byte_Array
  implicit val TagString: NBTView[String, NBTString]                  = NBTType.TAG_String
  implicit val TagCompound: NBTView[Map[String, NBTTag], NBTCompound] = NBTType.TAG_Compound
  implicit val TagIntArray: NBTView[IndexedSeq[Int], NBTIntArray]     = NBTType.TAG_Int_Array
  implicit val TagLongArray: NBTView[IndexedSeq[Long], NBTLongArray]  = NBTType.TAG_Long_Array

  implicit def listType[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit elementType: NBTType[ElemRepr, ElemNBT]
  ): NBTView[Seq[ElemNBT], NBTList[ElemRepr, ElemNBT]] = NBTType.listType[ElemRepr, ElemNBT]
}
trait LowPriorityNBTViews {
  implicit val BooleanView: NBTView[Boolean, NBTByte] = NBTBoolean
  implicit val UUIDView: NBTView[UUID, NBTCompound]   = NBTUUID
}

/**
  * A mixin view that provides extra methods allowing it to make a normal type look
  * like it has an nbt type.
  *
  * Example:
  * {{{
  *   NBTBoolean(false)
  * }}}
  *
  * @tparam Repr The type it represents
  * @tparam NBT The corresponding nbt type
  */
trait NBTViewCaseLike[Repr, NBT <: NBTTag] extends NBTView[Repr, NBT] { self =>
  def apply(v: Repr): NBT             = to(v)
  def unapply(arg: NBT): Option[Repr] = from(arg)

  override def imap[NewRepr](f: Repr => NewRepr, g: NewRepr => Repr): NBTView[NewRepr, NBT] =
    new NBTViewCaseLike[NewRepr, NBT] {
      override def to(v: NewRepr): NBT             = self.to(g(v))
      override def from(arg: NBT): Option[NewRepr] = self.from(arg).map(f)
    }

  override def imapOpt[NewRepr](f: Repr => Option[NewRepr], g: NewRepr => Repr): NBTViewCaseLike[NewRepr, NBT] =
    new NBTViewCaseLike[NewRepr, NBT] {
      override def to(v: NewRepr): NBT             = self.to(g(v))
      override def from(arg: NBT): Option[NewRepr] = self.from(arg).flatMap(f)
    }

  override def imapNbt[NewNBT <: NBTTag](f: NBT => NewNBT, g: NewNBT => NBT): NBTView[Repr, NewNBT] =
    new NBTViewCaseLike[Repr, NewNBT] {
      override def to(v: Repr): NewNBT             = f(self.to(v))
      override def from(arg: NewNBT): Option[Repr] = self.from(g(arg))
    }
}
object NBTViewCaseLike {
  def fromView[Repr, NBT <: NBTTag](view: NBTView[Repr, NBT]): NBTViewCaseLike[Repr, NBT] =
    new NBTViewCaseLike[Repr, NBT] {
      override def to(v: Repr): NBT             = view.to(v)
      override def from(arg: NBT): Option[Repr] = view.from(arg)
    }

  def forRepr[Repr] = new CaseViewForRepr[Repr]

  class CaseViewForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit view: NBTViewCaseLike[Repr, NBT]): NBTViewCaseLike[Repr, NBT] = view
  }
}

/**
  * A safer type of NBTView where [[NBTView.from]] can't fail.
  *
  * @tparam Repr The type it represents
  * @tparam NBT The corresponding nbt type
  */
trait SafeNBTView[Repr, NBT <: NBTTag] extends NBTView[Repr, NBT] with SafeNBTDeserializer[Repr, NBT] { self =>

  /**
    * Same as [[NBTView.modify]] except it uses [[SafeNBTDeserializer.fromSafe]]
    * so the result isn't an option.
    */
  def safeModify[NewRepr, NewNBT <: NBTTag](
      nbt: NBT
  )(f: Repr => NewRepr)(implicit newView: NBTView[NewRepr, NewNBT]): NewNBT =
    newView.to(f(fromSafe(nbt)))

  override def imap[NewRepr](f: Repr => NewRepr, g: NewRepr => Repr): SafeNBTView[NewRepr, NBT] =
    new SafeNBTView[NewRepr, NBT] {
      override def to(v: NewRepr): NBT         = self.to(g(v))
      override def fromSafe(arg: NBT): NewRepr = f(self.fromSafe(arg))
    }

  override def imapNbt[NewNBT <: NBTTag](f: NBT => NewNBT, g: NewNBT => NBT): SafeNBTView[Repr, NewNBT] =
    new SafeNBTView[Repr, NewNBT] {
      override def to(v: Repr): NewNBT         = f(self.to(v))
      override def fromSafe(arg: NewNBT): Repr = self.fromSafe(g(arg))
    }
}
object SafeNBTView {

  def forRepr[Repr] = new SafeViewForRepr[Repr]

  class SafeViewForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit view: SafeNBTView[Repr, NBT]): SafeNBTView[Repr, NBT] = view
  }
}

/**
  * A safe variant of the case like mixin.
  */
trait SafeNBTViewCaseLike[Repr, NBT <: NBTTag] extends SafeNBTView[Repr, NBT] with NBTViewCaseLike[Repr, NBT] { self =>

  override def imap[NewRepr](f: Repr => NewRepr, g: NewRepr => Repr): SafeNBTViewCaseLike[NewRepr, NBT] =
    new SafeNBTViewCaseLike[NewRepr, NBT] {
      override def to(v: NewRepr): NBT         = self.to(g(v))
      override def fromSafe(arg: NBT): NewRepr = f(self.fromSafe(arg))
    }

  override def imapNbt[NewNBT <: NBTTag](f: NBT => NewNBT, g: NewNBT => NBT): SafeNBTViewCaseLike[Repr, NewNBT] =
    new SafeNBTViewCaseLike[Repr, NewNBT] {
      override def to(v: Repr): NewNBT         = f(self.to(v))
      override def fromSafe(arg: NewNBT): Repr = self.fromSafe(g(arg))
    }
}
object SafeNBTViewCaseLike {

  def fromView[Repr, NBT <: NBTTag](view: SafeNBTView[Repr, NBT]): SafeNBTViewCaseLike[Repr, NBT] =
    new SafeNBTViewCaseLike[Repr, NBT] {
      override def to(v: Repr): NBT         = view.to(v)
      override def fromSafe(arg: NBT): Repr = view.fromSafe(arg)
    }

  def forRepr[Repr] = new SafeCaseViewForRepr[Repr]

  class SafeCaseViewForRepr[Repr](private val dummy: Boolean = false) extends AnyVal {
    def find[NBT <: NBTTag](implicit view: SafeNBTViewCaseLike[Repr, NBT]): SafeNBTViewCaseLike[Repr, NBT] = view
  }
}

/**
  * A special type of [[NBTView]] that represents a real nbt type.
  * It's also both safe and contains the numerical id of the type.
  */
sealed trait NBTType[Repr, NBT <: NBTTag.Aux[Repr]] extends SafeNBTView[Repr, NBT] {
  def id: Byte
  override def fromSafe(arg: NBT): Repr = arg.value
}

object NBTType {

  type Obj[Repr]      = NBTType[Repr, NBTTag.Aux[Repr]]
  type CovarObj[Repr] = NBTType[Repr, _ <: NBTTag.Aux[Repr]]

  def apply[Repr, NBT <: NBTTag.Aux[Repr]](implicit nbtType: NBTType[Repr, NBT]): NBTType[Repr, NBT] = nbtType

  val TagEnd: NBTType[Nothing, NBTEnd]                                = TAG_End
  implicit val TagByte: NBTType[Byte, NBTByte]                        = TAG_Byte
  implicit val TagShort: NBTType[Short, NBTShort]                     = TAG_Short
  implicit val TagInt: NBTType[Int, NBTInt]                           = TAG_Int
  implicit val TagLong: NBTType[Long, NBTLong]                        = TAG_Long
  implicit val TagFloat: NBTType[Float, NBTFloat]                     = TAG_Float
  implicit val TagDouble: NBTType[Double, NBTDouble]                  = TAG_Double
  implicit val TagByteArray: NBTType[IndexedSeq[Byte], NBTByteArray]  = TAG_Byte_Array
  implicit val TagString: NBTType[String, NBTString]                  = TAG_String
  implicit val TagCompound: NBTType[Map[String, NBTTag], NBTCompound] = TAG_Compound
  implicit val TagIntArray: NBTType[IndexedSeq[Int], NBTIntArray]     = TAG_Int_Array
  implicit val TagLongArray: NBTType[IndexedSeq[Long], NBTLongArray]  = TAG_Long_Array

  /**
    * Convert a numerical id to a [[NBTType]]
    */
  def fromId(i: Int): Option[NBTType.CovarObj[_]] = i match {
    case 0  => Some(TagEnd)
    case 1  => Some(TagByte)
    case 2  => Some(TagShort)
    case 3  => Some(TagInt)
    case 4  => Some(TagLong)
    case 5  => Some(TagFloat)
    case 6  => Some(TagDouble)
    case 7  => Some(TagByteArray)
    case 8  => Some(TagString)
    case 9  => Some(unsafe.TagList)
    case 10 => Some(TagCompound)
    case 11 => Some(TagIntArray)
    case 12 => Some(TagLongArray)
    case _  => None
  }

  private[typenbt] case object AnyTagType extends NBTType[Any, NBTTag.Aux[Any]] {
    override def id: Byte                    = throw new IllegalStateException("Tried to get ID for any tag")
    override def to(v: Any): NBTTag.Aux[Any] = throw new IllegalStateException("Tried to construct any tag")
  }

  //Official names for them
  case object TAG_End extends NBTType[Nothing, NBTEnd] {
    override def id: Byte                           = 0
    override def to(v: Nothing): NBTEnd             = throw new IllegalStateException("Tried to construct end tag")
    override def from(arg: NBTEnd): Option[Nothing] = throw new IllegalStateException("Tried to deconstruct end tag")
  }

  case object TAG_Byte extends NBTType[Byte, NBTByte] {
    override def id: Byte             = 1
    override def to(v: Byte): NBTByte = NBTByte(v)
  }

  case object TAG_Short extends NBTType[Short, NBTShort] {
    override def id: Byte               = 2
    override def to(v: Short): NBTShort = NBTShort(v)
  }

  case object TAG_Int extends NBTType[Int, NBTInt] {
    override def id: Byte           = 3
    override def to(v: Int): NBTInt = NBTInt(v)
  }

  case object TAG_Long extends NBTType[Long, NBTLong] {
    override def id: Byte             = 4
    override def to(v: Long): NBTLong = NBTLong(v)
  }

  case object TAG_Float extends NBTType[Float, NBTFloat] {
    override def id: Byte               = 5
    override def to(v: Float): NBTFloat = NBTFloat(v)
  }

  case object TAG_Double extends NBTType[Double, NBTDouble] {
    override def id: Byte                 = 6
    override def to(v: Double): NBTDouble = NBTDouble(v)
  }

  case object TAG_Byte_Array extends NBTType[IndexedSeq[Byte], NBTByteArray] {
    override def id: Byte                              = 7
    override def to(v: IndexedSeq[Byte]): NBTByteArray = NBTByteArray(v)
  }

  case object TAG_String extends NBTType[String, NBTString] {
    override def id: Byte                 = 8
    override def to(v: String): NBTString = NBTString(v)
  }

  case object TAG_Compound extends NBTType[Map[String, NBTTag], NBTCompound] {
    override def id: Byte                                = 10
    override def to(v: Map[String, NBTTag]): NBTCompound = NBTCompound(v)
  }

  case object TAG_Int_Array extends NBTType[IndexedSeq[Int], NBTIntArray] {
    override def id: Byte                            = 11
    override def to(v: IndexedSeq[Int]): NBTIntArray = NBTIntArray(v)
  }

  case object TAG_Long_Array extends NBTType[IndexedSeq[Long], NBTLongArray] {
    override def id: Byte                              = 12
    override def to(v: IndexedSeq[Long]): NBTLongArray = NBTLongArray(v)
  }

  implicit def listType[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit elementType: NBTType[ElemRepr, ElemNBT]
  ): NBTListType[ElemRepr, ElemNBT] =
    new NBTListType[ElemRepr, ElemNBT](elementType)

  //A raw list with no checks. If used wrong, this WILL cause problems
  private[typenbt] case object TAG_List extends NBTListType[Any, NBTTag.Aux[Any]](AnyTagType)
}

//We allow creating new list types for type sake
sealed class NBTListType[ElementRepr, ElementNBT <: NBTTag.Aux[ElementRepr]](
    val elementType: NBTType[ElementRepr, ElementNBT]
) extends NBTType[Seq[ElementNBT], NBTList[ElementRepr, ElementNBT]] {
  override def id: Byte = 9

  override def to(v: Seq[ElementNBT]): NBTList[ElementRepr, ElementNBT] =
    new NBTList[ElementRepr, ElementNBT](v)(this)

  override def equals(other: Any): Boolean = other match {
    case that: NBTListType[_, _] => elementType == that.elementType
    case _                       => false
  }

  override def hashCode(): Int = {
    val state = Seq(elementType)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
