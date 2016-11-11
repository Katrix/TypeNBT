import io.github.katrix.typenbt.nbt._
import io.github.katrix.typenbt.nbt.NBTView.ReprOps
import io.github.katrix.typenbt.parser.Mojangson

val nbtString = """{ }"""

def scalaCode(nbt: NBTTag): String = nbt match {
	case NBTCompound(tags) => s"NBTCompound(Map(${tags.map{case (name, tag) => s""""$name" -> ${scalaCode(tag)}"""}.mkString(", ")}))"
	case NBTList(tags) => s"NBTList(${tags.map(scalaCode(_)).toString()})"
	case NBTString(str) => s"""NBTString("$str")"""
	case NBTFloat(f) => s"NBTFloat(${f}F)"
	case _ => nbt.toString
}

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

scalaCode(compund)

Mojangson.Parser.skipWhitespace
val toMojangson = Mojangson.nbtToMojangson(compund)
Mojangson.mojangsonToNBT(toMojangson)
Mojangson.Parser.parse(Mojangson.Parser.nbtCompound, nbtString)