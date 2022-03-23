package net.katsstuff.typenbt.derivation.auto

import scala.compiletime.summonInline

import net.katsstuff.typenbt.derivation.NBTDerivation
import net.katsstuff.typenbt.derivation.NBTDerivation.NBTDerivationCodec
import net.katsstuff.typenbt.{NBTCompound, NBTView}
import perspective.derivation.HKDGeneric

implicit inline def deriveView[A]: NBTView[A, NBTCompound] =
  val gen = summonInline[HKDGeneric[A]]
  NBTDerivation.createView[A](using gen, summonInline[gen.Gen[NBTDerivationCodec]])
