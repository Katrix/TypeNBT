import io.github.katrix.typenbt.nbt._
import io.github.katrix.typenbt.nbt.NBTView.ReprOps
import io.github.katrix.typenbt.parser.Mojangson

val nbtString = """{ }"""

val list = NBTList((1 until 10).map(_.nbt))

val compund = NBTCompound()
  .set("byte", 3.toByte.nbt)
  .set("short", 3.toShort.nbt)
  .set("int", 5.nbt)
  .set("long", 3.toLong.nbt)
  .set("float", 3F.nbt)
  .set("double", 3D.nbt)
  .set("string", "test".nbt)
  .set("list", list)

val toMojangson = Mojangson.nbtToMojangson(compund)
Mojangson.mojangsonToNBT(toMojangson)
Mojangson.Parser.parse(Mojangson.Parser.nbtCompound, nbtString)