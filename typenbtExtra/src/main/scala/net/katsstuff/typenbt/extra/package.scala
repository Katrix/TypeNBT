package net.katsstuff.typenbt

import NBTCompound.NamedTag
import net.katsstuff.typenbt.extra.ExtraObjs._
import shapeless._
import shapeless.ops.hlist._

package object extra {

  implicit class ExtraNBTCompoundOps(private val compound: NBTCompound) extends AnyVal {

    /**
      * Creates a new [[NBTCompound]] with the hlist appended.
      * If there exists duplicate values it uses the second one.
      */
    def ++[Input <: HList, Mapped <: HList, Traversed](hList: Input)(
        implicit mapper: Mapper.Aux[tupleToNBT.type, Input, Mapped],
        toTraversable: ToTraversable.Aux[Mapped, Seq, Traversed],
        evidence: Traversed <:< NamedTag
    ): NBTCompound = compound.merge(NBTCompound.fromHList(hList))

    /**
      * Gets a value from this if it exists at the specified key,
      * and it can be converted to the specified value.
      */
    def getValue[Repr] = new GetValueNBTCompound[Repr](compound)

    /**
      * Same as [[NBTCompound.getNested]], but with a value instead of a [[NBTTag]].
      *
      * @see [[NBTCompound.getNested]]
      */
    def getNestedValue[Repr] = new GetRecursiveValueNBTCompound[Repr](compound)
  }

  implicit class ExtraNBTCompoundObjOps(private val compound: NBTCompound.type) extends AnyVal {

    def fromHList[Input <: HList, Mapped <: HList, Traversed](elements: Input)(
        implicit mapper: Mapper.Aux[tupleToNBT.type, Input, Mapped],
        toTraversable: ToTraversable.Aux[Mapped, Seq, Traversed],
        evidence: Traversed <:< NamedTag
    ) = NBTCompound(elements.map(tupleToNBT).to[Seq].toMap)
  }

  implicit def mapDeser[ElemRepr, ElemNBT <: NBTTag](
      implicit deser: NBTDeserializer[ElemRepr, ElemNBT],
      typeable: Typeable[ElemNBT]
  ): NBTDeserializer[Map[String, ElemRepr], NBTCompound] =
    (arg: NBTCompound) =>
      Some(
        for {
          (str, nbt) <- arg.value
          typed      <- typeable.cast(nbt).toSeq
          mapped     <- deser.from(typed).toSeq
        } yield str -> mapped
    )

  implicit def mapSafeDeser[ElemRepr, ElemNBT <: NBTTag](
      implicit deser: NBTDeserializer[ElemRepr, ElemNBT],
      typeable: Typeable[ElemNBT]
  ): SafeNBTDeserializer[Map[String, ElemRepr], NBTCompound] =
    (arg: NBTCompound) =>
      for {
        (str, nbt) <- arg.value
        typed      <- typeable.cast(nbt).toSeq
        mapped     <- deser.from(typed).toSeq
      } yield str -> mapped
}
