import fastparse._
import fastparse.MultiLineWhitespace._
import net.katsstuff.typenbt._

object UsingMojangson extends App {
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

  def errorMessage[T](p: P[_] => P[T], str: String) = {
    parse(str, p, verboseFailures = true) match {
      case Parsed.Success(value, index) => println(value)
      case e: Parsed.Failure => println(e.longMsg)
    }
  }

  errorMessage(Mojangson.MojangsonParser.wholeNbt(_), nbtString1)
}
