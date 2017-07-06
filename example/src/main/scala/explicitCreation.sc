import java.util.UUID

import net.katsstuff.typenbt._

NBTInt(5)
NBTCompound(Map("Hi" -> NBTInt(2), "There" -> NBTString("Nope")))

NBTList(Seq(NBTInt(5), NBTInt(3)))

//Custom types too
NBTBoolean(false)
NBTUUID(UUID.randomUUID())
