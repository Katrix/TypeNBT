import fastparse.core.{ParseError, Parsed, Parser}
import net.katsstuff.typenbt._

val nbtString1 = """{"4": [2}"""

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

val toMojangson = Mojangson.serialize(compund)
Mojangson.deserialize(toMojangson)

def errorMessage[T](p: Parser[T, Char, String], str: String) =
  ParseError(p.parse(str).asInstanceOf[Parsed.Failure[Char, String]]).getMessage

errorMessage(Mojangson.MojangsonParser.wholeNbt, nbtString1)

"dummy"