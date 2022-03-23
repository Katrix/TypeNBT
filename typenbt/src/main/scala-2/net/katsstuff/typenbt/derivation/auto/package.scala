package net.katsstuff.typenbt.derivation

import scala.language.experimental.macros

import net.katsstuff.typenbt.{NBTCompound, NBTView}

package object auto {
  implicit def deriveView[A]: NBTView[A, NBTCompound] = macro NBTDerivation.createView[A]
}
