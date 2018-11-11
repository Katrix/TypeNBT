package net.katsstuff.typenbt.extra

import net.katsstuff.typenbt._
import shapeless._

object ExtraObjs {

  object tupleToNBT extends Poly1 {
    implicit def apply[Repr, NBT <: NBTTag](implicit serializer: NBTSerializer[Repr, NBT]) =
      at[(String, Repr)] { case (name, value) => name -> serializer.to(value) }
  }
}
