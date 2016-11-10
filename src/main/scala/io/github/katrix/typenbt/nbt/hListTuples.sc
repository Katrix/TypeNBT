import java.util.UUID

import io.github.katrix.typenbt.nbt.{NBTCompound, NBTTag, NBTView}
import shapeless.UnaryTCConstraint._
import shapeless._
import shapeless.ops.hlist.{Mapper, ToTraversable}

type NamedValue[Value] = (String, Value)

object tupleToNBT extends Poly1 {
	implicit def apply[Repr, NBT <: NBTTag[_]](implicit view: NBTView.Aux[Repr, NBT]) =
		at[NamedValue[Repr]] { case (name, value) => name -> view(value) }
}

def toNBTSimple[Input <: HList : *->*[NamedValue]#λ, Mapped <: HList, Traversed](elements: Input)(
		implicit
		mapper: Mapper.Aux[tupleToNBT.type, Input, Mapped],
		toTraversable: ToTraversable.Aux[Mapped, Seq, Traversed],
		evidence: Traversed <:< (String, NBTTag[_])
) = NBTCompound(elements.map(tupleToNBT).to[Seq].toMap)

val exampleList = "first" -> 5 :: "second" -> false :: "third" -> "yay" :: "uuid?" -> UUID.randomUUID() :: HNil
toNBTSimple(exampleList)