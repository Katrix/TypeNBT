package net.katsstuff.typenbt.extra

import net.katsstuff.typenbt._
import shapeless._

object ExtraObjs {

  object tupleToNBT extends Poly1 {
    implicit def apply[Repr, NBT <: NBTTag](implicit serializer: NBTSerializer[Repr, NBT]) =
      at[(String, Repr)] { case (name, value) => name -> serializer.to(value) }
  }

  class GetValueNBTCompound[Repr](private val compound: NBTCompound) extends AnyVal {
    def apply[NBT <: NBTTag](
      key: String
    )(implicit deserializer: NBTDeserializer[Repr, NBT], tpe: Typeable[NBT]): Option[Repr] =
      compound.get(key).flatMap(nbt => tpe.cast(nbt).flatMap(deserializer.from))
  }

  class GetRecursiveValueNBTCompound[Repr](private val compound: NBTCompound) extends AnyVal {
    def apply[NBT <: NBTTag](
      keys: String*
    )(implicit deserializer: NBTDeserializer[Repr, NBT], tpe: Typeable[NBT]): Option[Repr] = {
      val tail = keys.tail
      if (tail == Nil) compound.getValue[Repr](keys.head)
      else
        compound.get(keys.head) match {
          case Some(compound: NBTCompound) => compound.getNestedValue[Repr](tail: _*)
          case _                           => None
        }
    }
  }
}
