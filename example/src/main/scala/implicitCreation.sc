import net.katsstuff.typenbt._
import shapeless._

5.nbt
"hi".nbt
false.nbt

val compound = NBTCompound(Map("Hi" -> NBTInt(2), "There" -> NBTString("Nope")))
compound.getValue[String]("There")

val intMap = Map("Hi" -> 2, "Int" -> 5).nbt

NBTCompound.fromHList("Hi" -> false :: "There" -> 5 :: HNil)

def toRepr[Repr, NBT <: NBTTag](nbt: NBT)(implicit view: NBTView[Repr, NBT]) = view.unapply(nbt)
val repr = toRepr[Map[String, Int], NBTCompound](intMap)

NBTView.forRepr[Map[String, String]].infer