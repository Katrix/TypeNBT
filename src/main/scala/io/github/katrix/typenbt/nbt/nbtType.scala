package io.github.katrix.typenbt.nbt

trait NBTType {
	type Repr
	type NBT <: NBTTag[Repr]
	def id: Byte
	def view: NBTView.Aux[Repr, NBT]
}

object NBTType extends NBTTypeInstances {

	type Aux[Repr0, NBT0 <: NBTTag[Repr0]] = NBTType { type Repr = Repr0; type NBT = NBT0 }
	type Lambda[Repr] = Aux[Repr, NBTTag[Repr]]

	def apply[Repr, NBT <: NBTTag[Repr]](implicit nbtType: NBTType.Aux[Repr, NBT]): NBTType.Aux[Repr, NBT] = nbtType
}

trait NBTTypeInstances {

	//Official names for them
	case object TAG_END extends NBTType {
		override type Repr = Nothing
		override type NBT = NBTTag[Nothing]
		override def id: Byte = 0
		override def view: NBTView.Aux[Repr, NBT] = NBTView.NothingView
	}

	implicit case object TAG_BYTE extends NBTType {
		override type Repr = Byte
		override type NBT = NBTByte
		override def id: Byte = 1
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ByteView
	}

	implicit case object TAG_SHORT extends NBTType {
		override type Repr = Short
		override type NBT = NBTShort
		override def id: Byte = 2
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ShortView
	}

	implicit case object TAG_INT extends NBTType {
		override type Repr = Int
		override type NBT = NBTInt
		override def id: Byte = 3
		override def view: NBTView.Aux[Repr, NBT] = NBTView.IntView
	}

	implicit case object TAG_LONG extends NBTType {
		override type Repr = Long
		override type NBT = NBTLong
		override def id: Byte = 4
		override def view: NBTView.Aux[Repr, NBT] = NBTView.LongView
	}

	implicit case object TAG_FLOAT extends NBTType {
		override type Repr = Float
		override type NBT = NBTFloat
		override def id: Byte = 5
		override def view: NBTView.Aux[Repr, NBT] = NBTView.FloatView
	}

	implicit case object TAG_DOUBLE extends NBTType {
		override type Repr = Double
		override type NBT = NBTDouble
		override def id: Byte = 6
		override def view: NBTView.Aux[Repr, NBT] = NBTView.DoubleView
	}

	implicit case object TAG_BYTE_ARRAY extends NBTType {
		override type Repr = IndexedSeq[Byte]
		override type NBT = NBTByteArray
		override def id: Byte = 7
		override def view: NBTView.Aux[Repr, NBT] = NBTView.IndexedSeqByteView
	}

	implicit case object TAG_STRING extends NBTType {
		override type Repr = String
		override type NBT = NBTString
		override def id: Byte = 8
		override def view: NBTView.Aux[Repr, NBT] = NBTView.StringView
	}

	implicit case object TAG_COMPOUND extends NBTType {
		override type Repr = Map[String, NBTTag[_]]
		override type NBT = NBTCompound
		override def id: Byte = 10
		override def view: NBTView.Aux[Repr, NBT] = NBTView.CompoundView
	}

	implicit case object TAG_INT_ARRAY extends NBTType {
		override type Repr = IndexedSeq[Int]
		override type NBT = NBTIntArray
		override def id: Byte = 11
		override def view: NBTView.Aux[Repr, NBT] = NBTView.IndexedSeqIntView
	}



	//We allow creating new list types for type sake
	abstract class NBTListType extends NBTType {
		type ElementRepr
		type ElementNBT <: NBTTag[ElementRepr]
		override type NBT = NBTList[ElementRepr, ElementNBT]
		override type Repr = Seq[ElementNBT]
		override final def id: Byte = 11
	}

	//A raw list with no checks. If used wrong, this WILL cause problems
	implicit case object TAG_LIST extends NBTListType {
		override type ElementRepr = Nothing
		override type ElementNBT = NBTTag[Nothing]
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListNothingView
	}

	implicit case object ListByte extends NBTListType {
		override type ElementRepr = Byte
		override type ElementNBT = NBTByte
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListByteView
	}

	implicit case object ListShort extends NBTListType {
		override type ElementRepr = Short
		override type ElementNBT = NBTShort
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListShortView
	}

	implicit case object ListInt extends NBTListType {
		override type ElementRepr = Int
		override type ElementNBT = NBTInt
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListIntView
	}

	implicit case object ListLong extends NBTListType {
		override type ElementRepr = Long
		override type ElementNBT = NBTLong
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListLongView
	}

	implicit case object ListFloat extends NBTListType {
		override type ElementRepr = Float
		override type ElementNBT = NBTFloat
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListFloatView
	}

	implicit case object ListDouble extends NBTListType {
		override type ElementRepr = Double
		override type ElementNBT = NBTDouble
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListDoubleView
	}

	implicit case object ListByteArray extends NBTListType {
		override type ElementRepr = IndexedSeq[Byte]
		override type ElementNBT = NBTByteArray
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListByteArrayView
	}

	implicit case object ListString extends NBTListType {
		override type ElementRepr = String
		override type ElementNBT = NBTString
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListStringView
	}

	implicit case object ListCompound extends NBTListType {
		override type ElementRepr = Map[String, NBTTag[_]]
		override type ElementNBT = NBTCompound
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListCompoundView
	}

	implicit case object ListIntArray extends NBTListType {
		override type ElementRepr = IndexedSeq[Int]
		override type ElementNBT = NBTIntArray
		override def view: NBTView.Aux[Repr, NBT] = NBTView.ListIntArrayView
	}

	def idToType(i: Int): Option[NBTType] = i match {
		case 0 => Some(TAG_END)
		case 1 => Some(TAG_BYTE)
		case 2 => Some(TAG_SHORT)
		case 3 => Some(TAG_INT)
		case 4 => Some(TAG_LONG)
		case 5 => Some(TAG_FLOAT)
		case 6 => Some(TAG_DOUBLE)
		case 7 => Some(TAG_BYTE_ARRAY)
		case 8 => Some(TAG_STRING)
		case 9 => Some(TAG_LIST)
		case 10 => Some(TAG_COMPOUND)
		case 11 => Some(TAG_INT_ARRAY)
		case _ => None
	}
}
