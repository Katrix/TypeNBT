import shapeless._

sealed trait Foo {
	type Repr
	def value: Repr
}

final case class FooInt(value: Int) extends Foo {type Repr = Int}
final case class FooMap(value: Map[String, Foo]) extends Foo {type Repr = Map[String, Foo]}

trait FooSerializer {
	type FooType <: Foo
	type Repr
	def write(repr: Repr): FooType
}
object FooSerializer {
	type Aux[FooType0 <: Foo, Repr0] = FooSerializer {type FooType = FooType0; type Repr = Repr0}
	def apply[FooType <: Foo, Repr](implicit serializer: Lazy[FooSerializer.Aux[FooType, Repr]]) = serializer.value
}

implicit object IntSerializer extends FooSerializer {
	override type FooType = FooInt
	override type Repr = Int
	override def write(repr: Repr): FooType = FooInt(repr)
}

implicit object MapSerializer extends FooSerializer {
	override type FooType = FooMap
	override type Repr = Map[String, Foo]
	override def write(repr: Repr): FooType = FooMap(repr)
}

implicit object hNilSerializer extends FooSerializer {
	override type FooType = FooMap
	override type Repr = HNil
	override def write(repr: Repr): FooType = FooMap(Map())
}

implicit def hListSerializer[Head, Tail <: HList, HeadFoo <: Foo](
		implicit
		headSerializer: Lazy[FooSerializer.Aux[HeadFoo, Head]],
		tailSerializer: Lazy[FooSerializer.Aux[FooMap, Tail]]
) = new FooSerializer {
	override type FooType = FooMap
	override type Repr = Head :: Tail
	override def write(repr: Repr): FooType = FooMap(Map(
		"head" -> headSerializer.value.write(repr.head),
		"tail" -> tailSerializer.value.write(repr.tail)
	))
}

hListSerializer[Int, Int :: HNil, FooInt].write(5 :: 6 :: HNil)