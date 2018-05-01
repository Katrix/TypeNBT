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

import scala.language.{existentials, higherKinds}

import shapeless._
import shapeless.labelled.FieldType

/**
  * A NBTView is a sort of Prism allowing you to see and modify NBT easier.
  * @tparam Repr The type it represents
  * @tparam NBT The corresponding nbt type
  */
trait NBTView[Repr, NBT <: NBTTag] {

  /**
    * Convert to NBT
    */
  def to(v: Repr): NBT

  /**
    * Convert from NBT
    */
  def from(arg: NBT): Option[Repr]

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

  def extend[NewRepr](f: Repr => Option[NewRepr], fInverse: NewRepr => Repr): NBTView[NewRepr, NBT] =
    new ExtendedNBTView(this, f, fInverse)
}

object NBTView extends NBTTypeInstances with NBTViewCaseCreator {

  class InferViewFromRepr[Repr](private val dummy: Unit = ()) extends AnyVal {
    def infer[NBT <: NBTTag](implicit infer: NBTView[Repr, NBT]): NBTView[Repr, NBT] = infer
  }

  def apply[Repr, NBT <: NBTTag](implicit view: NBTView[Repr, NBT]): NBTView[Repr, NBT] = view
  def forRepr[Repr]                                                                     = new InferViewFromRepr[Repr]

  class ReprOps[Repr](private val repr: Repr) extends AnyVal {
    def nbt[NBT <: NBTTag](implicit view: NBTView[Repr, NBT]): NBT = view.to(repr)
  }

  class NBTOps[NBT <: NBTTag](private val nbt: NBT) extends AnyVal {
    def set[Repr](repr: Repr)(implicit view: NBTView[Repr, NBT]): NBT = view.to(repr)
    def as[Repr](implicit view: NBTView[Repr, NBT]): Option[Repr]     = view.from(nbt)
    def safeAs[Repr](implicit view: SafeNBTView[Repr, NBT]): Repr     = view.fromSafe(nbt)
  }
}

/**
  * A view that provides extra methods allowing it to make a normal type look
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
trait NBTViewCaseLike[Repr, NBT <: NBTTag] extends NBTView[Repr, NBT] {
  def apply(v: Repr): NBT             = to(v)
  def unapply(arg: NBT): Option[Repr] = from(arg)
  override def extend[NewRepr](f: Repr => Option[NewRepr], fInverse: NewRepr => Repr): NBTViewCaseLike[NewRepr, NBT] =
    new ExtendedNBTView(this, f, fInverse) with NBTViewCaseLike[NewRepr, NBT]
}

/**
  * A safer type of NBTView where [[NBTView.from]] can't fail.
  *
  * @tparam Repr The type it represents
  * @tparam NBT The corresponding nbt type
  */
trait SafeNBTView[Repr, NBT <: NBTTag] extends NBTView[Repr, NBT] {

  /**
    * A safer version if [[NBTView.from]] that can't fail.
    */
  def fromSafe(arg: NBT): Repr

  override def from(arg: NBT): Option[Repr] = Some(fromSafe(arg))

  /**
    * Same as [[NBTView.modify]] except it uses [[SafeNBTView.fromSafe]]
    * so the result isn't an option.
    */
  def safeModify[NewRepr, NewNBT <: NBTTag](
      nbt: NBT
  )(f: Repr => NewRepr)(implicit newView: NBTView[NewRepr, NewNBT]): NewNBT =
    newView.to(f(fromSafe(nbt)))

  def safeExtend[NewRepr](f: Repr => NewRepr, fInverse: NewRepr => Repr): SafeNBTView[NewRepr, NBT] =
    new SafeExtendedNBTView(this, f, fInverse)
}

class ExtendedNBTView[ExtendRepr, Repr, NBT <: NBTTag](
    underlying: NBTView[Repr, NBT],
    f: Repr => Option[ExtendRepr],
    fInverse: ExtendRepr => Repr
) extends NBTView[ExtendRepr, NBT] {

  override def to(v: ExtendRepr): NBT             = underlying.to(fInverse(v))
  override def from(arg: NBT): Option[ExtendRepr] = underlying.from(arg).flatMap(f)
  override def extend[NewRepr](g: ExtendRepr => Option[NewRepr], gInverse: NewRepr => ExtendRepr): NBTView[NewRepr, NBT] =
    new ExtendedNBTView[NewRepr, Repr, NBT](underlying, f.andThen(_.flatMap(g)), fInverse.compose(gInverse))
}

class SafeExtendedNBTView[ExtendRepr, Repr, NBT <: NBTTag](
    underlying: SafeNBTView[Repr, NBT],
    f: Repr => ExtendRepr,
    fInverse: ExtendRepr => Repr
) extends SafeNBTView[ExtendRepr, NBT] {
  override def fromSafe(arg: NBT): ExtendRepr = f(underlying.fromSafe(arg))
  override def to(v: ExtendRepr): NBT         = underlying.to(fInverse(v))

  override def extend[NewRepr](g: ExtendRepr => Option[NewRepr], gInverse: NewRepr => ExtendRepr): NBTView[NewRepr, NBT] =
    new ExtendedNBTView[NewRepr, Repr, NBT](underlying, f.andThen(g), fInverse.compose(gInverse))
  override def safeExtend[NewRepr](g: ExtendRepr => NewRepr, gInverse: NewRepr => ExtendRepr): SafeNBTView[NewRepr, NBT] =
    new SafeExtendedNBTView[NewRepr, Repr, NBT](underlying, f.andThen(g), fInverse.compose(gInverse))
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

  class InferTypeFromRepr[Repr](val dummy: Unit) extends AnyVal {
    def infer[NBT <: NBTTag.Aux[Repr]](implicit extract: NBTType[Repr, NBT]): NBTType[Repr, NBT] = extract
  }

  def apply[Repr, NBT <: NBTTag.Aux[Repr]](implicit nbtType: NBTType[Repr, NBT]): NBTType[Repr, NBT] = nbtType
  def forRepr[Repr]                                                                                  = new InferTypeFromRepr[Repr]

  /**
    * Convert a numerical id to a [[NBTType]]
    */
  def fromId(i: Int): Option[NBTType.CovarObj[_]] = i match {
    case 0  => Some(NBTView.TagEnd)
    case 1  => Some(NBTView.TagByte)
    case 2  => Some(NBTView.TagShort)
    case 3  => Some(NBTView.TagInt)
    case 4  => Some(NBTView.TagLong)
    case 5  => Some(NBTView.TagFloat)
    case 6  => Some(NBTView.TagDouble)
    case 7  => Some(NBTView.TagByteArray)
    case 8  => Some(NBTView.TagString)
    case 9  => Some(unsafe.TagList)
    case 10 => Some(NBTView.TagCompound)
    case 11 => Some(NBTView.TagIntArray)
    case 12 => Some(NBTView.TagLongArray)
    case _  => None
  }
}

//We allow creating new list types for type sake
sealed class NBTListType[ElementRepr, ElementNBT <: NBTTag.Aux[ElementRepr]](
    val elementType: NBTType[ElementRepr, ElementNBT]
) extends NBTType[Seq[ElementNBT], NBTList[ElementRepr, ElementNBT]] {
  override def id: Byte = 11

  override def to(v: Seq[ElementNBT]): NBTList[ElementRepr, ElementNBT] =
    new NBTList[ElementRepr, ElementNBT](v)(this)
}

trait NBTTypeInstances extends NBTViewInstances {

  val TagEnd: TAG_End.type              = TAG_End
  val TagByte: TAG_Byte.type            = TAG_Byte
  val TagShort: TAG_Short.type          = TAG_Short
  val TagInt: TAG_Int.type              = TAG_Int
  val TagLong: TAG_Long.type            = TAG_Long
  val TagFloat: TAG_Float.type          = TAG_Float
  val TagDouble: TAG_Double.type        = TAG_Double
  val TagByteArray: TAG_Byte_Array.type = TAG_Byte_Array
  val TagString: TAG_String.type        = TAG_String
  val TagCompound: TAG_Compound.type    = TAG_Compound
  val TagIntArray: TAG_Int_Array.type   = TAG_Int_Array
  val TagLongArray: TAG_Long_Array.type = TAG_Long_Array

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

  implicit case object TAG_Byte extends NBTType[Byte, NBTByte] {
    override def id: Byte             = 1
    override def to(v: Byte): NBTByte = NBTByte(v)
  }

  implicit case object TAG_Short extends NBTType[Short, NBTShort] {
    override def id: Byte               = 2
    override def to(v: Short): NBTShort = NBTShort(v)
  }

  implicit case object TAG_Int extends NBTType[Int, NBTInt] {
    override def id: Byte           = 3
    override def to(v: Int): NBTInt = NBTInt(v)
  }

  implicit case object TAG_Long extends NBTType[Long, NBTLong] {
    override def id: Byte             = 4
    override def to(v: Long): NBTLong = NBTLong(v)
  }

  implicit case object TAG_Float extends NBTType[Float, NBTFloat] {
    override def id: Byte               = 5
    override def to(v: Float): NBTFloat = NBTFloat(v)
  }

  implicit case object TAG_Double extends NBTType[Double, NBTDouble] {
    override def id: Byte                 = 6
    override def to(v: Double): NBTDouble = NBTDouble(v)
  }

  implicit case object TAG_Byte_Array extends NBTType[IndexedSeq[Byte], NBTByteArray] {
    override def id: Byte                              = 7
    override def to(v: IndexedSeq[Byte]): NBTByteArray = NBTByteArray(v)
  }

  implicit case object TAG_String extends NBTType[String, NBTString] {
    override def id: Byte                 = 8
    override def to(v: String): NBTString = NBTString(v)
  }

  implicit case object TAG_Compound extends NBTType[Map[String, NBTTag], NBTCompound] {
    override def id: Byte                                = 10
    override def to(v: Map[String, NBTTag]): NBTCompound = NBTCompound(v)
  }

  implicit case object TAG_Int_Array extends NBTType[IndexedSeq[Int], NBTIntArray] {
    override def id: Byte                            = 11
    override def to(v: IndexedSeq[Int]): NBTIntArray = NBTIntArray(v)
  }

  implicit case object TAG_Long_Array extends NBTType[IndexedSeq[Long], NBTLongArray] {
    override def id: Byte                              = 12
    override def to(v: IndexedSeq[Long]): NBTLongArray = NBTLongArray(v)
  }

  implicit def listType[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit elementType: NBTType[ElemRepr, ElemNBT]
  ): NBTListType[ElemRepr, ElemNBT] =
    new NBTListType[ElemRepr, ElemNBT](elementType)

  //A raw list with no checks. If used wrong, this WILL cause problems
  private[typenbt] case object TAG_List extends NBTListType[Any, NBTTag.Aux[Any]](NBTView.AnyTagType)
}

trait NBTViewInstances extends LowPriorityViewInstances {

  implicit val BooleanView: NBTBoolean.type = NBTBoolean
  implicit val UUIDView: NBTUUID.type       = NBTUUID

  implicit def mapView[ElemRepr, ElemNBT <: NBTTag](
      implicit view: NBTView[ElemRepr, ElemNBT],
      typeable: Typeable[ElemNBT]
  ): SafeNBTView[Map[String, ElemRepr], NBTCompound] =
    new SafeNBTView[Map[String, ElemRepr], NBTCompound] {
      override def to(v: Map[String, ElemRepr]): NBTCompound = {
        val mapped = v.mapValues(view.to)
        NBTCompound(mapped)
      }

      override def fromSafe(arg: NBTCompound): Map[String, ElemRepr] =
        for {
          (str, nbt) <- arg.value
          typed      <- typeable.cast(nbt).toSeq
          mapped     <- view.from(typed).toSeq
        } yield str -> mapped
    }
}

trait LowPriorityViewInstances {

  implicit def seqView[ListNBTRepr, SeqRepr, ListNBT <: NBTTag.Aux[ListNBTRepr]](
      implicit view: NBTView[SeqRepr, ListNBT],
      listType: NBTListType[ListNBTRepr, ListNBT]
  ): SafeNBTView[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] =
    new SafeNBTView[Seq[SeqRepr], NBTList[ListNBTRepr, ListNBT]] {
      override def to(v: Seq[SeqRepr]): NBTList[ListNBTRepr, ListNBT]         = NBTList[ListNBTRepr, ListNBT](v.map(view.to))
      override def fromSafe(arg: NBTList[ListNBTRepr, ListNBT]): Seq[SeqRepr] = arg.value.flatMap(view.from)
    }
}

trait NBTViewCaseCreator {

  implicit val hNilView: SafeNBTView[HNil, NBTCompound] = new SafeNBTView[HNil, NBTCompound] {
    override def to(v: HNil): NBTCompound         = NBTCompound()
    override def fromSafe(arg: NBTCompound): HNil = HNil
  }

  implicit val cNilView: NBTView[CNil, NBTCompound] = new NBTView[CNil, NBTCompound] {
    override def to(v: CNil): NBTCompound             = v.impossible
    override def from(arg: NBTCompound): Option[CNil] = sys.error("cnil")
  }

  //FIXME: HeadNBT is problematic as Lazy does not work with multiple type parameters
  implicit def hConsView[Name <: Symbol, Head, Tail <: HList, HeadNBT <: NBTTag](
      implicit name: Witness.Aux[Name],
      vh: Lazy[NBTView[Head, HeadNBT]],
      vt: Lazy[NBTView[Tail, NBTCompound]],
      tpe: Typeable[HeadNBT]
  ): NBTView[FieldType[Name, Head] :: Tail, NBTCompound] = new NBTView[FieldType[Name, Head] :: Tail, NBTCompound] {

    override def to(v: FieldType[Name, Head] :: Tail): NBTCompound =
      vt.value.to(v.tail).set(name.value.name, vh.value.to(v.head))

    override def from(arg: NBTCompound): Option[FieldType[Name, Head] :: Tail] =
      for {
        head <- arg.get(name.value.name).flatMap(tpe.cast(_).flatMap(vh.value.from))
        tail <- vt.value.from(arg)
      } yield labelled.field[Name](head) :: tail
  }

  //FIXME: LeftNBT is problematic as Lazy does not work with multiple type parameters
  implicit def coProdView[Name <: Symbol, Left, Right <: Coproduct, LeftNBT <: NBTTag](
      implicit name: Witness.Aux[Name],
      vl: Lazy[NBTView[Left, LeftNBT]],
      vr: Lazy[NBTView[Right, NBTCompound]],
      tpe: Typeable[LeftNBT]
  ): NBTView[FieldType[Name, Left] :+: Right, NBTCompound] = new NBTView[FieldType[Name, Left] :+: Right, NBTCompound] {

    override def to(v: FieldType[Name, Left] :+: Right): NBTCompound = v match {
      case Inl(l) => NBTCompound(Map(name.value.name -> vl.value.to(l)))
      case Inr(r) => vr.value.to(r)
    }

    override def from(arg: NBTCompound): Option[FieldType[Name, Left] :+: Right] =
      arg.get(name.value.name) match {
        case Some(tag) => tpe.cast(tag).flatMap(vl.value.from(_).map(l => Inl(labelled.field[Name](l))))
        case None      => vr.value.from(arg).map(Inr(_))
      }
  }

  implicit def caseToView[A, HList](
      implicit gen: LabelledGeneric.Aux[A, HList],
      ser: Lazy[NBTView[HList, NBTCompound]]
  ): NBTView[A, NBTCompound] = new NBTView[A, NBTCompound] {
    override def to(v: A): NBTCompound             = ser.value.to(gen.to(v))
    override def from(arg: NBTCompound): Option[A] = ser.value.from(arg).map(gen.from)
  }
}
