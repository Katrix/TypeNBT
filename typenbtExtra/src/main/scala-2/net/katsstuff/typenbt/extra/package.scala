package net.katsstuff.typenbt

import NBTCompound.NamedTag
import net.katsstuff.typenbt.extra.ExtraObjs._
import shapeless._
import shapeless.ops.hlist._

package object extra {

  implicit class ExtraNBTCompoundOps(private val compound: NBTCompound) extends AnyVal {

    /**
      * Creates a new [[NBTCompound]] with the hlist appended. If there exists
      * duplicate values it uses the second one.
      */
    def ++[Input <: HList, NbtMapped <: HList, Traversed](hList: Input)(
        implicit mapper: Mapper.Aux[tupleToNBT.type, Input, NbtMapped],
        toTraversable: ToTraversable.Aux[NbtMapped, Seq, Traversed],
        evidence: Traversed <:< NamedTag
    ): NBTCompound = compound.merge(NBTCompound.fromHList(hList))
  }

  implicit class ExtraNBTCompoundObjOps(private val compound: NBTCompound.type) extends AnyVal {

    def fromHList[Input <: HList, NbtMapped <: HList, Traversed](elements: Input)(
        implicit mapper: Mapper.Aux[tupleToNBT.type, Input, NbtMapped],
        toTraversable: ToTraversable.Aux[NbtMapped, Seq, Traversed],
        evidence: Traversed <:< NamedTag
    ) = NBTCompound(elements.map(tupleToNBT).to[Seq].toMap)
  }

  // noinspection ConvertExpressionToSAM
  implicit def mapDeser[ElemRepr, ElemNBT <: NBTTag](
      implicit deser: NBTDeserializer[ElemRepr, ElemNBT],
      typeable: Typeable[ElemNBT]
  ): NBTDeserializer[Map[String, ElemRepr], NBTCompound] = new NBTDeserializer[Map[String, ElemRepr], NBTCompound] {
    override def from(arg: NBTCompound): Option[Map[String, ElemRepr]] = Some(
      for {
        (str, nbt) <- arg.value
        typed      <- typeable.cast(nbt).toSeq
        mapped     <- deser.from(typed).toSeq
      } yield str -> mapped
    )
  }

  // noinspection ConvertExpressionToSAM
  implicit def mapSafeDeser[ElemRepr, ElemNBT <: NBTTag](
      implicit deser: NBTDeserializer[ElemRepr, ElemNBT],
      typeable: Typeable[ElemNBT]
  ): SafeNBTDeserializer[Map[String, ElemRepr], NBTCompound] =
    new SafeNBTDeserializer[Map[String, ElemRepr], NBTCompound] {
      override def fromSafe(arg: NBTCompound): Map[String, ElemRepr] =
        for {
          (str, nbt) <- arg.value
          typed      <- typeable.cast(nbt).toSeq
          mapped     <- deser.from(typed).toSeq
        } yield str -> mapped
    }
}
