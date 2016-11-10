import io.github.katrix.typenbt.nbt.{NBTInt, NBTTag}
import shapeless._

trait NBTTypeNat {
	type NBT <: NBTTag[Repr]
	type Repr
	type Id <: Nat
	def id: Id
}
object NBTTypeNat {
	type Aux[NBT0 <: NBTTag[Repr0], Repr0, Id0 <: Nat] = NBTTypeNat {
		type NBT = NBT0
		type Repr = Repr0
		type Id = Id0
	}

	final class NatExtractor[N <: Nat] {
		def apply[Repr, NBT <: NBTTag[Repr]](implicit nbtType: NBTTypeNat.Aux[NBT, Repr, N]) = nbtType
	}

	final class NotNatExtractor[Repr, NBT <: NBTTag[Repr]] {
		def apply[N <: Nat](implicit nbtType: NBTTypeNat.Aux[NBT, Repr, N]) = nbtType
	}

	def apply[Repr, NBT <: NBTTag[Repr], N <: Nat](implicit nbtType: NBTTypeNat.Aux[NBT, Repr, N]) = nbtType

	def nat[N <: Nat] = new NatExtractor[N]
	def notNat[Repr, NBT <: NBTTag[Repr]] = new NotNatExtractor[Repr, NBT]
	def repr[Repr] = new NotNatExtractor[Repr, NBTTag[Repr]]

	implicit object TAG_INT extends NBTTypeNat {
		override type NBT = NBTInt#Self
		override type Repr = Int
		override type Id = Nat._3
		override def id: Id = Nat._3
	}

	implicit object TAG_STRING extends NBTTypeNat {
		override type NBT = NBTTag[String]
		override type Repr = String
		override type Id = Nat._8
		override def id: Id = Nat._8
	}
}

NBTTypeNat[Int, NBTInt#Self, Nat._3]
NBTTypeNat.nat[Nat._3].apply
NBTTypeNat.notNat[Int, NBTInt#Self].apply
NBTTypeNat.repr[String].apply

//Nat.apply(id)