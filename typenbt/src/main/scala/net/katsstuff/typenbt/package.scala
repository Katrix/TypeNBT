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
package net.katsstuff

import java.util.UUID

import scala.language.implicitConversions

import net.katsstuff.typenbt.NBTType.InferTypeFromRepr
import net.katsstuff.typenbt.NBTView.InferViewFromRepr

package object typenbt {

  implicit def applyViewInfer[Repr, NBT <: NBTTag](
      infer: InferViewFromRepr[Repr]
  )(implicit extract: NBTView[Repr, NBT]): NBTView[Repr, NBT] =
    infer.infer[NBT]

  implicit def applyTypeInfer[Repr, NBT <: NBTTag.Aux[Repr]](infer: InferTypeFromRepr[Repr])(
      implicit extract: NBTType[Repr, NBT]
  ): NBTType[Repr, NBT] = infer.infer[NBT]

  implicit def reprOps[Repr](repr: Repr):       NBTView.ReprOps[Repr] = NBTView.ReprOps(repr)
  implicit def nbtOps[NBT <: NBTTag](nbt: NBT): NBTView.NBTOps[NBT]   = NBTView.NBTOps(nbt)

  object NBTBoolean extends NBTViewCaseLike[Boolean, NBTByte] {
    override def to(v: Boolean):     NBTByte         = NBTByte(if (v) 1 else 0)
    override def from(arg: NBTByte): Option[Boolean] = Some(arg.value == 1)
  }

  object NBTUUID extends NBTViewCaseLike[UUID, NBTCompound] {
    override def from(arg: NBTCompound): Option[UUID] =
      arg.get("Most").flatMap {
        case NBTLong(mostSign) =>
          arg.get("Least").flatMap {
            case NBTLong(leastSign) => Some(new UUID(mostSign, leastSign))
            case _                  => None
          }
        case _ => None
      }

    override def to(v: UUID): NBTCompound =
      NBTCompound(Map("Most" -> NBTLong(v.getMostSignificantBits), "Least" -> NBTLong(v.getLeastSignificantBits)))
  }
}
