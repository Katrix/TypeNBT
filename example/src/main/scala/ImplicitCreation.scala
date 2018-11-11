import java.util.UUID

import net.katsstuff.typenbt._

object ImplicitCreation extends App {
  5.nbt
  Seq(1.nbt, 2.nbt, 3.nbt).nbt
  Map("Hi" -> 2, "Int" -> 5).nbt

  //Custom types too
  false.nbt
  UUID.randomUUID().nbt
}
