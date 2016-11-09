import java.util.UUID

import io.github.katrix.typenbt.nbt._
import io.github.katrix.typenbt.nbt.NBTView.ReprOps
import io.github.katrix.typenbt.misc.AST
import io.github.katrix.typenbt.parser.Mojangson

val nbtString = """{ }"""

def scalaCode(nbt: NBTTag[_]): String = nbt match {
	case NBTCompound(tags) => s"NBTCompound(Map(${tags.map{case (name, tag) => s""""$name" -> ${scalaCode(tag)}"""}.mkString(", ")}))"
	case NBTList(tags) => s"NBTList(${tags.map(scalaCode(_)).toString()})"
	case NBTString(str) => s"""NBTString("$str")"""
	case NBTFloat(f) => s"NBTFloat(${f}F)"
	case _ => nbt.toString
}

trait Foo[Repr] {
	def value: Repr
}
case class FooInt(value: Int) extends Foo[Int]

trait FooSerializer[Repr, FooType <: Foo[Repr]] {
	def create(v: Repr): FooType
	def extract(foo: FooType): Option[Repr]
}
implicit case object FooIntSerializer extends FooSerializer[Int, Foo[Int]] {
	override def create(v: Int): Foo[Int] = FooInt(v)
	override def extract(foo: Foo[Int]): Option[Int] = Some(foo.value)
}

def fromFoo[Repr, FooType <: Foo[Repr]](foo: FooType)
	(implicit serializer: FooSerializer[Repr, FooType]): Option[Repr] = serializer.extract(foo)

def toFoo[Repr, FooType <: Foo[Repr]](repr: Repr)
	(implicit serializer: FooSerializer[Repr, FooType]): FooType = serializer.create(repr)

def nbtType[Repr, NBT <: NBTTag[Repr]](nbt: NBT)
	(implicit nbtType: NBTType.Aux[Repr, NBT]): NBTType.Aux[Repr, NBT] = nbtType

def fromNbt[Repr, NBT <: NBTTag[_]](nbt: NBT)
	(implicit nbtView: NBTView.Aux[Repr, NBT]) = nbtView.unapply(nbt)

def toNbt[Repr, NBT <: NBTTag[_]](repr: Repr)
	(implicit nbtView: NBTView.Aux[Repr, NBT]) = nbtView(repr)

fromNbt(NBTInt(5))
toNbt(5)
NBTCompound().setValue("uuid", UUID.randomUUID())
//fromFoo(FooInt(1)) //Doesn't work. Implicit not found

//nbtType(NBTInt(5))

//val list = NBTList[NBTInt, Int]((1 until 10).map(_.nbt))
//NBTList[Int, NBTInt](Seq(NBTInt(1)))

val compund = NBTCompound()
	.set("byte", 3.toByte.nbt)
	.set("short", 3.toShort.nbt)
	.set("int", 5.nbt)
	.set("long", 3.toLong.nbt)
	.set("float", 3F.nbt)
	.set("double", 3D.nbt)
	.set("string", "test".nbt)
	//.set("list", list)

scalaCode(compund)

val compoundAst = AST(compund)

Mojangson.Parser.skipWhitespace
Mojangson.nbtToMojangson(compund)
Mojangson.Parser.parse(Mojangson.Parser.nbtCompound, nbtString)