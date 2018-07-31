package net.katsstuff.typenbt.extra
import net.katsstuff.typenbt.{NBTCompound, NBTView}

object DerivTest {
  case class Foo(i: Int, str: String, bar: Double)
  val fooView: NBTView[Foo, NBTCompound] = net.katsstuff.typenbt.derivation.semiauto.deriveView[Foo]

}
