package net.katsstuff.typenbt.extra

import net.katsstuff.typenbt.*

import perspective.derivation.*

extension (tag: NBTCompound)
  /**
    * Creates a new [[NBTCompound]] with the hlist appended. If there exists
    * duplicate values it uses the second one.
    */
  inline def ++[Inputs <: Tuple](input: Inputs): NBTCompound =
    tag.merge(NBTCompound.fromTuple(input))

type NBTTupleSerializers[T <: Tuple] <: Tuple = T match {
  case (String, h) *: t => NBTSerializer[h, _] *: NBTTupleSerializers[t]
  case EmptyTuple => EmptyTuple
}

extension (companion: NBTCompound.type)
  inline def fromTuple[Inputs <: Tuple](elements: Inputs): NBTCompound = {
    val serializers = scala.compiletime.summonAll[NBTTupleSerializers[Inputs]]
    val nbtElements = (elements: Product).productIterator.zip(serializers.productIterator).map {
      case (t, rawSerializer) =>
        val (key, value) = t.asInstanceOf[(String, Any)]
        val serializer = rawSerializer.asInstanceOf[NBTSerializer[Any, _]]
        (key, serializer.to(value): NBTTag)
    }.toMap

    NBTCompound(nbtElements)
  }


val test = NBTCompound.fromTuple(Tuple1(("foo", "bar")))
