package net.katsstuff.typenbt.derivation

import scala.compiletime.*
import scala.deriving.Mirror

import net.katsstuff.typenbt.*
import perspective.*
import perspective.derivation.*

object NBTDerivation {

  trait NBTDerivationCodec[A] {
    type NBT <: NBTTag
    def serializer: NBTSerializer[A, NBT]
    def deserializer: NBTDeserializer[A, NBT]
    def refiner: NBTRefiner[NBT]
  }
  object NBTDerivationCodec:
    type Aux[A, NBT0 <: NBTTag] = NBTDerivationCodec[A] { type NBT = NBT0 }
    given [A, NBT0 <: NBTTag](
        using serializer0: NBTSerializer[A, NBT0],
        deserializer0: NBTDeserializer[A, NBT0],
        refiner0: NBTRefiner[NBT0]
    ): Aux[A, NBT0] = new NBTDerivationCodec[A]:
      override type NBT = NBT0
      override def serializer: NBTSerializer[A, NBT]     = serializer0
      override def deserializer: NBTDeserializer[A, NBT] = deserializer0
      override def refiner: NBTRefiner[NBT]              = refiner0

  inline def createView[A](using gen: HKDGeneric[A], codecs: gen.Gen[NBTDerivationCodec]): NBTViewCompound[A] =
    inline gen match
      case gen: HKDProductGeneric.Aux[A, gen.Gen] => createProductView(using gen, codecs)
      case gen: HKDSumGeneric.Aux[A, gen.Gen]     => createSumView(using gen, codecs)

  inline def createProductView[A](
      using gen: HKDProductGeneric[A],
      codecs: gen.Gen[NBTDerivationCodec]
  ): NBTViewCompound[A] = {
    val codecs = summonInline[gen.Gen[NBTDerivationCodec]]

    new NBTViewCompound[A] {
      def to(v: A): NBTCompound =
        val entries = gen
          .to(v)
          .map2Const(codecs)([Z] => (value: Z, codec: NBTDerivationCodec[Z]) => codec.serializer.to(value): NBTTag)
          .map2Const[Const[gen.Names], (String, NBTTag)](gen.names)(
            [Z] => (tag: NBTTag, name: gen.Names) => (name: String, tag)
          )
          .toListK
          .toMap

        NBTCompound(entries)

      def from(arg: NBTCompound): Option[A] =
        gen.names
          .map2K(codecs)(
            [Z] =>
              (name: gen.Names, codec: NBTDerivationCodec[Z]) =>
                arg.get(name).flatMap(codec.refiner.refine).flatMap(codec.deserializer.from)
          )
          .sequenceIdK
          .map(gen.from)
    }
  }

  inline def createSumView[A](using gen: HKDSumGeneric[A], codecs: gen.Gen[NBTDerivationCodec]): NBTViewCompound[A] = {
    new NBTViewCompound[A] {
      def to(v: A): NBTCompound =
        NBTCompound(
          "$type" -> NBTString(gen.indexToName(gen.indexOf(v))),
          "$value" ->
            gen
              .to(v)
              .map2Const(codecs)(
                [Z] => (value: Option[Z], codec: NBTDerivationCodec[Z]) => value.map(codec.serializer.to(_): NBTTag)
              )
              .indexK(gen.indexOf(v))
              .get
        )

      def from(arg: NBTCompound): Option[A] =
        for
          typeNameStr <- arg.getValue[String]("$type")
          typeName    <- gen.stringToName(typeNameStr)
          index = gen.nameToIndex(typeName)
          codec = codecs.indexK(index)
          rawTag <- arg.get("$value")
          tag    <- codec.refiner.refine(rawTag)
          res    <- codec.deserializer.from(tag)
        yield res
    }
  }
}
