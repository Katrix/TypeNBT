package net.katsstuff.typenbt

/** A typeclass that helps matching on a raw [[NBTTag]]. */
trait NBTRefiner[+NBT] {

  def refine(tag: NBTTag): Option[NBT]
}
//noinspection ConvertExpressionToSAM
object NBTRefiner {
  def apply[NBT](implicit refiner: NBTRefiner[NBT]): NBTRefiner[NBT] = refiner

  def fromDeserializer[NBT <: NBTTag](
      deserializer: NBTDeserializer[_, NBT]
  )(implicit refiner: NBTRefiner[NBT]): NBTRefiner[NBT] =
    refiner

  implicit val byteRefiner: NBTRefiner[NBTByte] = new NBTRefiner[NBTByte] {
    override def refine(tag: NBTTag): Option[NBTByte] = tag match {
      case res: NBTByte => Some(res)
      case _            => None
    }
  }

  implicit val shortRefiner: NBTRefiner[NBTShort] = new NBTRefiner[NBTShort] {
    override def refine(tag: NBTTag): Option[NBTShort] = tag match {
      case res: NBTShort => Some(res)
      case _             => None
    }
  }

  implicit val intRefiner: NBTRefiner[NBTInt] = new NBTRefiner[NBTInt] {
    override def refine(tag: NBTTag): Option[NBTInt] = tag match {
      case res: NBTInt => Some(res)
      case _           => None
    }
  }

  implicit val longRefiner: NBTRefiner[NBTLong] = new NBTRefiner[NBTLong] {
    override def refine(tag: NBTTag): Option[NBTLong] = tag match {
      case res: NBTLong => Some(res)
      case _            => None
    }
  }

  implicit val floatRefiner: NBTRefiner[NBTFloat] = new NBTRefiner[NBTFloat] {
    override def refine(tag: NBTTag): Option[NBTFloat] = tag match {
      case res: NBTFloat => Some(res)
      case _             => None
    }
  }

  implicit val doubleRefiner: NBTRefiner[NBTDouble] = new NBTRefiner[NBTDouble] {
    override def refine(tag: NBTTag): Option[NBTDouble] = tag match {
      case res: NBTDouble => Some(res)
      case _              => None
    }
  }

  implicit val byteArrayRefiner: NBTRefiner[NBTByteArray] = new NBTRefiner[NBTByteArray] {
    override def refine(tag: NBTTag): Option[NBTByteArray] = tag match {
      case res: NBTByteArray => Some(res)
      case _                 => None
    }
  }

  implicit val stringRefiner: NBTRefiner[NBTString] = new NBTRefiner[NBTString] {
    override def refine(tag: NBTTag): Option[NBTString] = tag match {
      case res: NBTString => Some(res)
      case _              => None
    }
  }

  implicit val compoundRefiner: NBTRefiner[NBTCompound] = new NBTRefiner[NBTCompound] {
    override def refine(tag: NBTTag): Option[NBTCompound] = tag match {
      case res: NBTCompound => Some(res)
      case _                => None
    }
  }

  implicit val intArrayRefiner: NBTRefiner[NBTIntArray] = new NBTRefiner[NBTIntArray] {
    override def refine(tag: NBTTag): Option[NBTIntArray] = tag match {
      case res: NBTIntArray => Some(res)
      case _                => None
    }
  }

  implicit val longArrayRefiner: NBTRefiner[NBTLongArray] = new NBTRefiner[NBTLongArray] {
    override def refine(tag: NBTTag): Option[NBTLongArray] = tag match {
      case res: NBTLongArray => Some(res)
      case _                 => None
    }
  }

  // A bit hacky but hopefully it's better than nothing
  implicit def listRefiner[ElemRepr, ElemNBT <: NBTTag.Aux[ElemRepr]](
      implicit listType: NBTListType[ElemRepr, ElemNBT]
  ): NBTRefiner[NBTList[ElemRepr, ElemNBT]] = new NBTRefiner[NBTList[ElemRepr, ElemNBT]] {
    override def refine(tag: NBTTag): Option[NBTList[ElemRepr, ElemNBT]] = tag match {
      case list: NBTList[_, _] if list.nbtType == listType => Some(list.asInstanceOf[NBTList[ElemRepr, ElemNBT]])
      case _                                               => None
    }
  }
}
