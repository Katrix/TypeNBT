import net.katsstuff.typenbt._

val li = NBTView.listType[Int, NBTInt]
implicit val lli = NBTView.listType(li)

lli.toNbt(
  Seq(
    NBTList(Seq(1.nbt, 2.nbt, 3.nbt)),
    NBTList(Seq(4.nbt, 5.nbt, 6.nbt))
  )
)

NBTList(Seq(NBTList(Seq(1.nbt, 2.nbt, 3.nbt))))