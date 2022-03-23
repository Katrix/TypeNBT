package net.katsstuff.typenbt

import net.katsstuff.typenbt.derivation.NBTDerivation.NBTDerivationCodec
import perspective.derivation.HKDGeneric

trait NBTViewCompoundCompat {
  inline given derived[A](using gen: HKDGeneric[A], codecs: gen.Gen[NBTDerivationCodec]): NBTViewCompound[A] =
    derivation.NBTDerivation.createView[A]
}
