import io.github.katrix.typenbt.nbt._

import shapeless._
import shapeless.syntax.singleton._

class ImplicitTester1[Name <: Symbol, Head, Tail <: HList] {
  def apply[HeadNBT <: NBTTag](implicit name: Witness.Aux[Name],
                               vh:            NBTView.Aux[Head, HeadNBT],
                               vt:            Lazy[NBTView.Aux[Tail, NBTCompound]],
                               tpe:           Typeable[HeadNBT]) = tpe
}

class ImplicitTester2[Name <: Symbol, Head, Tail <: HList] {
  def apply[HeadNBT <: NBTTag](implicit name: Witness.Aux[Name],
                               vh:            Lazy[NBTView.Aux[Head, HeadNBT]],
                               vt:            Lazy[NBTView.Aux[Tail, NBTCompound]],
                               tpe:           Typeable[HeadNBT]) = tpe
}

val symbolSingleton = 'first.narrow
new ImplicitTester1[symbolSingleton.type, String, HNil].apply
//new ImplicitTester2[symbolSingleton.type, String, HNil].apply

case class Name(first: String)
case class Person(name: Name, age: Int)

val nameGen = LabelledGeneric[Name]
//NBTView.caseToView[Name, nameGen.Repr]