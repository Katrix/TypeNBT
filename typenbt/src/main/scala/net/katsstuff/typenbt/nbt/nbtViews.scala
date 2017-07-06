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

import java.util.UUID

import scala.language.higherKinds

import shapeless.Typeable

trait NBTView[Repr, NBT <: NBTTag] {
  def apply(v:     Repr): NBT
  def unapply(arg: NBT):  Option[Repr]

  def modify[NewRepr, NewNBT <: NBTTag](nbt: NBT)(f: Repr => NewRepr)(implicit newView: NBTView[NewRepr, NewNBT]): Option[NewNBT] =
    this.unapply(nbt).map(a => newView(f(a)))
}
object NBTView extends NBTTypeInstances with NBTViewCaseCreator {

  sealed class InferViewFromRepr[Repr] {
    def infer[NBT <: NBTTag](implicit infer: NBTView[Repr, NBT]): NBTView[Repr, NBT] = infer
  }

  def apply[Repr, NBT <: NBTTag](implicit view: NBTView[Repr, NBT]): NBTView[Repr, NBT] = view
  def forRepr[Repr] = new InferViewFromRepr[Repr]

  implicit class ReprOps[Repr](private val repr: Repr) extends AnyVal {
    def nbt[NBT <: NBTTag](implicit view: NBTView[Repr, NBT]): NBT = view(repr)
  }

  implicit class NBTOps[NBT <: NBTTag](private val nbt: NBT) extends AnyVal {
    def set[Repr](repr: Repr)(implicit view: NBTView[Repr, NBT]): NBT = view.apply(repr)

    //FIXME: Need to specify Repr type in f
    def modify[Repr, NewRepr, NewNBT <: NBTTag](f: Repr => NewRepr)(implicit view: NBTView[Repr, NBT], newView: NBTView[NewRepr, NewNBT]): Option[NewNBT] =
      view.modify(nbt)(f)(newView)
  }
}

trait NBTViewInstances extends LowPriorityViewInstances {

  implicit case object BooleanView extends NBTView[Boolean, NBTByte] {
    override def apply(v:     Boolean): NBTByte         = NBTByte(if (v) 1 else 0)
    override def unapply(arg: NBTByte): Option[Boolean] = Some(arg.value == 1)
  }

  implicit case object UUIDView extends NBTView[UUID, NBTCompound] {
    override def unapply(arg: NBTCompound): Option[UUID] =
      arg.get("Most").flatMap {
        case NBTLong(mostSign) =>
          arg.get("Least").flatMap {
            case NBTLong(leastSign) => Some(new UUID(mostSign, leastSign))
            case _                  => None
          }
        case _ => None
      }

    override def apply(v: UUID): NBTCompound =
      NBTCompound(Map("Most" -> NBTLong(v.getMostSignificantBits), "Least" -> NBTLong(v.getLeastSignificantBits)))
  }

  implicit def mapView[ElemRepr, ElemNBT <: NBTTag](implicit view: NBTView[ElemRepr, ElemNBT], typeable: Typeable[ElemNBT]) =
    new NBTView[Map[String, ElemRepr], NBTCompound] {
      override def apply(v: Map[String, ElemRepr]): NBTCompound = {
        val mapped = v.mapValues(view.apply)
        NBTCompound(mapped)
      }
      override def unapply(arg: NBTCompound): Option[Map[String, ElemRepr]] = {
        val res = for {
          (str, nbt) <- arg.value
          typed      <- typeable.cast(nbt).toSeq
          mapped     <- view.unapply(typed).toSeq
        } yield str -> mapped

        Some(res)
      }
    }
}

trait LowPriorityViewInstances {

  implicit def seqView[RawRepr, ElemRepr, ElemNBT <: NBTTag.Aux[RawRepr]](implicit view: NBTView[ElemRepr, ElemNBT],
      listType:      NBTView.NBTListType[RawRepr, ElemNBT]) = new NBTView[Seq[ElemRepr], NBTList[RawRepr, ElemNBT]] {
    override def apply(v:     Seq[ElemRepr]): NBTList[RawRepr, ElemNBT] = NBTList[RawRepr, ElemNBT](v.map(view.apply))
    override def unapply(arg: NBTList[RawRepr, ElemNBT]): Option[Seq[ElemRepr]] = {
      val mapped = arg.value.map(view.unapply)
      Some(mapped.flatten)
    }
  }
}

trait NBTViewCaseCreator {

  /*
  implicit case object EmptyProduct extends NBTView {
    override type Repr = HNil
    override type NBT  = NBTCompound
    override def apply(v:     HNil): NBT          = NBTCompound()
    override def unapply(arg: NBT):  Option[HNil] = Some(HNil)
  }

  implicit case object EmptyCoproduct extends NBTView {
    override type Repr = CNil
    override type NBT  = NBTCompound
    override def apply(v:     Repr): NBT          = throw new IllegalStateException
    override def unapply(arg: NBT):  Option[Repr] = throw new IllegalStateException
  }

  implicit def product[Name <: Symbol, Head, Tail <: HList, HeadNBT <: NBTTag](implicit name: Witness.Aux[Name],
                                                                               vh:            Lazy[NBTView.Aux[Head, HeadNBT]],
                                                                               vt:            Lazy[NBTView.Aux[Tail, NBTCompound]],
                                                                               tpe:           Typeable[HeadNBT]): NBTView = new NBTView {
    override type Repr = FieldType[Name, Head] :: Tail
    override type NBT  = NBTCompound
    override def apply(v:     Repr): NBT = vt.value(v.tail).set(name.value.name, vh.value(v.head))
    override def unapply(arg: NBT): Option[Repr] = {
      val head = arg.get(name.value.name).flatMap(tpe.cast(_).flatMap(vh.value.unapply))
      val tail = vt.value.unapply(arg)

      head.flatMap(h => tail.map(t => field[Name](h) :: t))
    }
  }

  implicit def coproduct[Name <: Symbol, Left, Right <: Coproduct, LeftNBT <: NBTTag](implicit name: Witness.Aux[Name],
                                                                                      vl:            Lazy[NBTView.Aux[Left, LeftNBT]],
                                                                                      vr:            Lazy[NBTView.Aux[Right, NBTCompound]],
                                                                                      tpe:           Typeable[LeftNBT]): NBTView = new NBTView {
    override type Repr = FieldType[Name, Left] :+: Right
    override type NBT  = NBTCompound
    override def apply(v: Repr): NBT = v match {
      case Inl(l) => NBTCompound(Map(name.value.name -> vl.value(l)))
      case Inr(r) => vr.value(r)
    }
    override def unapply(arg: NBT): Option[Repr] =
      arg.asInstanceOf[NBTCompound].get(name.value.name) match {
        case Some(tag) => tpe.cast(tag).flatMap(vl.value.unapply(_).map(l => Inl(field[Name](l))))
        case None      => vr.value.unapply(arg).map(Inr(_))
      }
  }

  implicit def caseToView[A, HList](implicit gen: LabelledGeneric.Aux[A, HList], ser: Lazy[NBTView.Aux[HList, NBTCompound]]): NBTView = new NBTView {
    override type Repr = A
    override type NBT  = NBTCompound
    override def apply(v:     Repr): NBT          = ser.value(gen.to(v))
    override def unapply(arg: NBT):  Option[Repr] = ser.value.unapply(arg).map(gen.from)
  }
 */
}
