import java.util.UUID

import net.katsstuff.typenbt._

val compound = NBTCompound(Map("Hi" -> 2.nbt, "There" -> "Nope".nbt, "Nested" -> NBTCompound(Map("InNested" -> false.nbt))))
compound.getValue[String]("There")
compound.setValue("Foo", 5)
compound.getRecursive("Nested", "InNested")
compound.getRecursiveValue[Byte]("Nested", "InNested")

val withUUID = compound.setUUID("UUID", UUID.randomUUID())
withUUID.getUUID("UUID")

compound.merge(NBTUUID(UUID.randomUUID()))

val newNBT = compound("5") = 5.nbt
compound("5")