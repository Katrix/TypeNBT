/*
 * This file is part of TypeNBT, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2018 Katrix
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

import org.scalatest.{FunSuite, Matchers}

import net.katsstuff.typenbt._

class CorrectViewTest extends FunSuite with Matchers {

  def getView[NBT <: NBTTag] = new getView[NBT]

  class getView[NBT <: NBTTag] {
    def apply[Repr](implicit view: NBTView[Repr, NBT]): NBTView[Repr, NBT] = view
  }

  test("The view for NBTByte should be NBTType.TagByte") {
    assert(getView[NBTByte].apply === NBTType.TagByte)
  }

  test("The view for NBTShort should be NBTType.TagShort") {
    assert(getView[NBTShort].apply === NBTType.TagShort)
  }

  test("The view for NBTInt should be NBTType.TagInt") {
    assert(getView[NBTInt].apply === NBTType.TagInt)
  }

  test("The view for NBTLong should be NBTType.TagLong") {
    assert(getView[NBTLong].apply === NBTType.TagLong)
  }

  test("The view for NBTFloat should be NBTType.TagFloat") {
    assert(getView[NBTFloat].apply === NBTType.TagFloat)
  }

  test("The view for NBTDouble should be NBTType.TagDouble") {
    assert(getView[NBTDouble].apply === NBTType.TagDouble)
  }

  test("The view for NBTByteArray should be NBTType.TagByteArray") {
    assert(getView[NBTByteArray].apply === NBTType.TagByteArray)
  }

  test("The view for NBTString should be NBTType.TagString") {
    assert(getView[NBTString].apply === NBTType.TagString)
  }

  test("The view for NBTCompound should be NBTType.TagCompound") {
    assert(getView[NBTCompound].apply === NBTType.TagCompound)
  }

  test("The view for NBTIntArray should be NBTType.TagIntArray") {
    assert(getView[NBTIntArray].apply === NBTType.TagIntArray)
  }

  test("The view for NBTLongArray should be NBTType.TagLongArray") {
    assert(getView[NBTLongArray].apply === NBTType.TagLongArray)
  }
}
