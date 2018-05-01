import net.katsstuff.typenbt._

val nbt = 5.nbt
nbt.set(10)

nbt.modify((i: Int) => i.toString)

val byteNbt = false.nbt
byteNbt.set(1.toByte)
byteNbt.set(2.toByte)
byteNbt.set(true)

nbt.modify(_.toString)
byteNbt.modify(_.toString)