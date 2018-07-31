package net.katsstuff.typenbt

/**
  * A typeclass that helps matching on a raw [[NBTTag]].
  */
trait NBTRefiner[+NBT] {

  def refine(tag: NBTTag): Option[NBT]
}
object NBTRefiner {
  def apply[NBT](implicit refiner: NBTRefiner[NBT]): NBTRefiner[NBT] = refiner

  def fromDeserializer[NBT <: NBTTag](
      deserializer: NBTDeserializer[_, NBT]
  )(implicit refiner: NBTRefiner[NBT]): NBTRefiner[NBT] =
    refiner

  implicit val byteRefiner: NBTRefiner[NBTByte] = {
    case res: NBTByte => Some(res)
    case _            => None
  }

  implicit val shortRefiner: NBTRefiner[NBTShort] = {
    case res: NBTShort => Some(res)
    case _             => None
  }

  implicit val intRefiner: NBTRefiner[NBTInt] = {
    case res: NBTInt => Some(res)
    case _           => None
  }

  implicit val longRefiner: NBTRefiner[NBTLong] = {
    case res: NBTLong => Some(res)
    case _            => None
  }

  implicit val floatRefiner: NBTRefiner[NBTFloat] = {
    case res: NBTFloat => Some(res)
    case _             => None
  }

  implicit val doubleRefiner: NBTRefiner[NBTDouble] = {
    case res: NBTDouble => Some(res)
    case _              => None
  }

  implicit val byteArrayRefiner: NBTRefiner[NBTByteArray] = {
    case res: NBTByteArray => Some(res)
    case _                 => None
  }

  implicit val stringRefiner: NBTRefiner[NBTString] = {
    case res: NBTString => Some(res)
    case _              => None
  }

  implicit val compoundRefiner: NBTRefiner[NBTCompound] = {
    case res: NBTCompound => Some(res)
    case _                => None
  }

  implicit val intArrayRefiner: NBTRefiner[NBTIntArray] = {
    case res: NBTIntArray => Some(res)
    case _                => None
  }

  implicit val longArrayRefiner: NBTRefiner[NBTLongArray] = {
    case res: NBTLongArray => Some(res)
    case _                 => None
  }

  //A bit hacky but hopefully it's better than nothing
  implicit def listRefiner[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit listType: NBTListType[ElemRepr, ElemNBT]
  ): NBTRefiner[NBTList[ElemRepr, ElemNBT]] = {
    case list: NBTList[_, _] if list.nbtType == listType => Some(list.asInstanceOf[NBTList[ElemRepr, ElemNBT]])
    case _                                               => None
  }
}
