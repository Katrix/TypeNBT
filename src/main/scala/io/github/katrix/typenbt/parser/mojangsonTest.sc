import io.github.katrix.typenbt.nbt._
import io.github.katrix.typenbt.nbt.NBTView._
import io.github.katrix.typenbt.misc.AST
import io.github.katrix.typenbt.parser.Mojangson

val nbtString = """{ }"""

val list = NBTList[NBTInt, Int](Seq(
	1.nbt[NBTInt],
	2.nbt[NBTInt],
	3.nbt[NBTInt],
	4.nbt[NBTInt],
	5.nbt[NBTInt],
	6.nbt[NBTInt],
	7.nbt[NBTInt],
	8.nbt[NBTInt],
	9.nbt[NBTInt]
))

val compund = NBTCompound()
	.set("byte", 3.toByte.nbt[NBTByte])
	.set("short", 3.toShort.nbt[NBTShort])
	.set("int", 5.nbt[NBTInt])
	.set("long", 3.nbt[Long, NBTLong])
	.set("float", 3F.nbt[NBTFloat])
	.set("double", 3D.nbt[NBTDouble])
	.set("string", "test".nbt[NBTString])
	.set("list", list)

val compoundAst = AST(compund)

Mojangson.Parser.skipWhitespace
Mojangson.nbtToMojangson(compund)
Mojangson.Parser.parse(Mojangson.Parser.nbtCompound, nbtString)