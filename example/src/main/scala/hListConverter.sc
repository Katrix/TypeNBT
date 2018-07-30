import java.util.UUID

import net.katsstuff.typenbt._
import net.katsstuff.typenbt.extra._
import shapeless._

val exampleList = "first" -> 5 :: "second" -> "yay" :: "bool" -> true :: "uuid" -> UUID.randomUUID() :: HNil
val compound    = NBTCompound.fromHList(exampleList)

compound ++ ("third" -> 10 :: "nay" -> false :: HNil)
