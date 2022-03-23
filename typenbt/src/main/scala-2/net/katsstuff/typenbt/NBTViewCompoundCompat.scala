package net.katsstuff.typenbt

import net.katsstuff.typenbt.derivation.NBTDerivation

import scala.language.experimental.macros

trait NBTViewCompoundCompat {
  def derived[A]: NBTView[A, NBTCompound] = macro NBTDerivation.createView[A]
}