import net.katsstuff.typenbt.nbt._

val boolTag = NBTBoolean(false)
val NBTBoolean(bool) = boolTag

NBTCompound(Map("hi" -> false))
Map("rawrr" -> false).nbt
NBTView[Map[String, Boolean], NBTCompound]

//NBTList(Seq(false, true, false))
//Seq(false, true, false).nbt
NBTView[Seq[Boolean], NBTList[Byte, NBTByte]]