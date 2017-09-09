/*
 * This file is part of TypeNBT, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2016 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package net.katsstuff.typenbt.parser

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck._

import net.katsstuff.typenbt._

trait NBTGenerator {

  final val Epsilon = 1E5
  val saneDouble: Gen[Double] = Gen.choose(-Epsilon, Epsilon)
  val saneFloat:  Gen[Float]  = Gen.choose(-Epsilon.toFloat, Epsilon.toFloat)

  val genNbtByte:      Gen[NBTByte]      = for (v <- arbitrary[Byte]) yield NBTByte(v)
  val genNbtShort:     Gen[NBTShort]     = for (v <- arbitrary[Short]) yield NBTShort(v)
  val genNbtInt:       Gen[NBTInt]       = for (v <- arbitrary[Int]) yield NBTInt(v)
  val genNbtLong:      Gen[NBTLong]      = for (v <- arbitrary[Long]) yield NBTLong(v)
  val genNbtFloat:     Gen[NBTFloat]     = for (v <- saneFloat) yield NBTFloat(v)
  val genNbtDouble:    Gen[NBTDouble]    = for (v <- saneDouble) yield NBTDouble(v)
  val genNbtByteArray: Gen[NBTByteArray] = for (v <- arbitrary[IndexedSeq[Byte]]) yield NBTByteArray(v)
  val genNbtString:    Gen[NBTString]    = for (v <- Gen.alphaNumStr.suchThat(_.length < 80)) yield NBTString(v)
  val genNbtIntArray:  Gen[NBTIntArray]  = for (v <- arbitrary[IndexedSeq[Int]]) yield NBTIntArray(v)

  implicit val arbitraryNbtByte:      Arbitrary[NBTByte]      = Arbitrary(genNbtByte)
  implicit val arbitraryNbtShort:     Arbitrary[NBTShort]     = Arbitrary(genNbtShort)
  implicit val arbitraryNbtInt:       Arbitrary[NBTInt]       = Arbitrary(genNbtInt)
  implicit val arbitraryNbtLong:      Arbitrary[NBTLong]      = Arbitrary(genNbtLong)
  implicit val arbitraryNbtFloat:     Arbitrary[NBTFloat]     = Arbitrary(genNbtFloat)
  implicit val arbitraryNbtDouble:    Arbitrary[NBTDouble]    = Arbitrary(genNbtDouble)
  implicit val arbitraryNbtByteArray: Arbitrary[NBTByteArray] = Arbitrary(genNbtByteArray)
  implicit val arbitraryNbtString:    Arbitrary[NBTString]    = Arbitrary(genNbtString)
  implicit val arbitraryNbtIntArray:  Arbitrary[NBTIntArray]  = Arbitrary(genNbtIntArray)

  def genNbtList[Repr, NBT <: NBTTag.Aux[Repr]: Arbitrary](
      implicit nbtType: NBTListType[Repr, NBT]
  ): Gen[NBTList[Repr, NBT]] =
    for {
      l <- oneOf(Gen.const(NBTList()), nonEmptyNbtList[Repr, NBT](NBTList()))
    } yield l

  def nonEmptyNbtList[Repr, NBT <: NBTTag.Aux[Repr]: Arbitrary](
      list: NBTList[Repr, NBT]
  )(implicit nbtType: NBTListType[Repr, NBT]): Gen[NBTList[Repr, NBT]] =
    for {
      v <- arbitrary[NBT]
      l <- oneOf(Gen.const(list :+ v), nonEmptyNbtList[Repr, NBT](list :+ v))
    } yield l

  def genNonEmptyNbtCompound(compound: NBTCompound): Gen[NBTCompound] =
    for {
      k <- Gen.alphaNumStr.suchThat(string => string.nonEmpty && !string.matches("""\s"""))
      v <- arbitrary[NBTTag]
      m <- oneOf(Gen.const(compound), genNonEmptyNbtCompound(compound.set(k, v)))
    } yield m

  val genNbtCompound: Gen[NBTCompound] = for {
    m <- oneOf(Gen.const(NBTCompound()), genNonEmptyNbtCompound(NBTCompound()))
  } yield m

  implicit val arbitraryNbtCompound: Arbitrary[NBTCompound] = Arbitrary(genNbtCompound)

  val genNbtListType: Gen[NBTList[_, _ <: NBTTag]] = oneOf(
    genNbtList[Byte, NBTByte],
    genNbtList[Short, NBTShort],
    genNbtList[Int, NBTInt],
    genNbtList[Long, NBTLong],
    genNbtList[Float, NBTFloat],
    genNbtList[Double, NBTDouble],
    genNbtList[IndexedSeq[Byte], NBTByteArray],
    genNbtList[String, NBTString],
    genNbtList[IndexedSeq[Int], NBTIntArray],
    genNbtList[Map[String, NBTTag], NBTCompound]
  )

  val genNonEmptyNbtList: Gen[NBTList[_, _ <: NBTTag]] = oneOf(
    nonEmptyNbtList[Byte, NBTByte](NBTList()),
    nonEmptyNbtList[Short, NBTShort](NBTList()),
    nonEmptyNbtList[Int, NBTInt](NBTList()),
    nonEmptyNbtList[Long, NBTLong](NBTList()),
    nonEmptyNbtList[Float, NBTFloat](NBTList()),
    nonEmptyNbtList[Double, NBTDouble](NBTList()),
    nonEmptyNbtList[IndexedSeq[Byte], NBTByteArray](NBTList()),
    nonEmptyNbtList[String, NBTString](NBTList()),
    nonEmptyNbtList[IndexedSeq[Int], NBTIntArray](NBTList()),
    nonEmptyNbtList[Map[String, NBTTag], NBTCompound](NBTList())
  )

  val genNbt: Gen[NBTTag] = oneOf(
    genNbtByte,
    genNbtShort,
    genNbtInt,
    genNbtLong,
    genNbtFloat,
    genNbtDouble,
    genNbtByteArray,
    genNbtString,
    genNbtListType,
    genNbtCompound,
    genNbtIntArray
  )

  implicit val arbitraryNbt: Arbitrary[NBTTag] = Arbitrary(genNbt)
}
