import io.github.katrix.typenbt.nbt.{NBTCompound, NBTInt, NBTView}
import io.github.katrix.typenbt.parser.Mojangson

case class Name(first: String, last: String)
case class Person(name: Name, age: Int)

val setCompound = NBTCompound().setValue[NBTCompound, Person]("person", Person(Name("Ola", "Norman"), 5))
setCompound.getValue[NBTCompound, Person]("person")
Mojangson.nbtToMojangson(setCompound)

val invalidView = NBTView[NBTInt]