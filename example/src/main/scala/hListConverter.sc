import io.github.katrix.typenbt.nbt.{NBTCompound, NBTTag, NBTView}
import shapeless._
import shapeless.ops.hlist.{Mapper, ToTraversable}


object tupleToNBT extends Poly1 {
  implicit def apply[Repr, NBT <: NBTTag](implicit view: NBTView.Aux[Repr, NBT]) =
    at[(String, Repr)] { case (name, value) => name -> view(value) }
}

def toNBT[Input <: HList, Mapped <: HList, Traversed](elements: Input)(
    implicit mapper: Mapper.Aux[tupleToNBT.type, Input, Mapped],
    toTraversable: ToTraversable.Aux[Mapped, Seq, Traversed],
    evidence: Traversed <:< (String, NBTTag)) =
  NBTCompound(elements.map(tupleToNBT).to[Seq].toMap)

val exampleList = "first" -> 5 :: "second" -> "yay" :: HNil
toNBT(exampleList)