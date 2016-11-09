trait Foo[Repr] {
	def value: Repr
}
case class FooInt(value: Int) extends Foo[Int]

trait FooSerializer {
	type Repr
	type FooType <: Foo[_]
	def create(v: Repr): FooType
	def extract(foo: Foo[Repr]): Option[Repr]
}
object FooSerializer {
	type Aux[R, F <: Foo[_]] = FooSerializer { type Repr = R; type FooType = F }
}

implicit case object FooIntSerializer extends FooSerializer {
	type Repr = Int
	type FooType = FooInt
	override def create(v: Int): FooInt = FooInt(v)
	override def extract(foo: Foo[Int]): Option[Int] = Some(foo.value)
}

def fromFoo[Repr, FooType <: Foo[Repr]](foo: FooType with Foo[Repr])
	(implicit serializer: FooSerializer.Aux[Repr, FooType]): Option[Repr] = {
	serializer.extract(foo)
}

def toFoo[Repr, FooType <: Foo[Repr]](repr: Repr)
	(implicit serializer: FooSerializer.Aux[Repr, FooType]): FooType = {
	serializer.create(repr)
}



println(fromFoo(toFoo(5)))