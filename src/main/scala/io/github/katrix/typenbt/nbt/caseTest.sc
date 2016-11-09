import io.github.katrix.typenbt.nbt._
import NBTView.ReprOps
import scala.reflect.runtime.universe._

case class Name(first: String)
case class Person(name: Name, age: Int)


reify(5.nbt)
reify("hi".nbt)
reify(NBTView[String, NBTString])
//reify(Name("Hi").nbt)

//val personTo = NBTView[Name, NBTCompound]

//personTo(Person(Name("Olaf", "Normann"), 20))