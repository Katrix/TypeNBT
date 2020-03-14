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
package net.katsstuff.typenbt

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ListCompiles extends AnyFunSuite with Matchers {

  test("NBTList with normal int tag compiles") {
    """
      |import net.katsstuff.typenbt.{NBTInt, NBTList}
      |NBTList(Seq(NBTInt(5), NBTInt(4), NBTInt(3)))
    """.stripMargin should compile
  }

  test("NBTList with ints doesn't typeCheck") {
    """
      |import net.katsstuff.typenbt.NBTList
      |NBTList(Seq(5, 4, 3))
    """.stripMargin shouldNot typeCheck
  }

  test("Nested NBTList with int should compile") {
    """
      |import net.katsstuff.typenbt.{NBTInt, NBTList}
      |NBTList(Seq(NBTList(Seq(NBTInt(1)))))
    """.stripMargin should compile
  }

}
