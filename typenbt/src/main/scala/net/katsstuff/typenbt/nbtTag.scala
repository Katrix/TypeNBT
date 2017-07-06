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
package net.katsstuff.typenbt

import java.util.UUID

import scala.annotation.tailrec

import shapeless.ops.hlist.{Mapper, ToTraversable}
import shapeless.{HList, Poly1, Typeable}

sealed trait NBTTag {

  /**
		* The value that this [[NBTTag]] holds.
		*/
  type Repr
  type Self <: NBTTag.Aux[Repr]

  /**
		* The value of this [[NBTTag]]
		*/
  def value: Repr

  /**
		* The type of this [[NBTTag]]
		*/
  def nbtType: NBTType[Repr, Self]
}

object NBTTag {
  type Aux[Repr0] = NBTTag { type Repr = Repr0 }
}

sealed trait NBTEnd extends NBTTag {
  override type Repr = Nothing
  override type Self = NBTEnd
  override def nbtType: NBTType[Repr, Self] = NBTView.TagEnd
}
final case class NBTByte(value: Byte) extends NBTTag {
  override type Repr = Byte
  override type Self = NBTByte
  override def nbtType: NBTType[Repr, Self] = NBTView.TagByte
}
final case class NBTShort(value: Short) extends NBTTag {
  override type Repr = Short
  override type Self = NBTShort
  override def nbtType: NBTType[Repr, Self] = NBTView.TagShort
}
final case class NBTInt(value: Int) extends NBTTag {
  override type Repr = Int
  override type Self = NBTInt
  override def nbtType: NBTType[Repr, Self] = NBTView.TagInt
}
final case class NBTLong(value: Long) extends NBTTag {
  override type Repr = Long
  override type Self = NBTLong
  override def nbtType: NBTType[Repr, Self] = NBTView.TagLong
}
final case class NBTFloat(value: Float) extends NBTTag {
  override type Repr = Float
  override type Self = NBTFloat
  override def nbtType: NBTType[Repr, Self] = NBTView.TagFloat
}
final case class NBTDouble(value: Double) extends NBTTag {
  override type Repr = Double
  override type Self = NBTDouble
  override def nbtType: NBTType[Repr, Self] = NBTView.TagDouble
}
final case class NBTByteArray(value: IndexedSeq[Byte]) extends NBTTag {
  override type Repr = IndexedSeq[Byte]
  override type Self = NBTByteArray
  override def nbtType: NBTType[Repr, Self] = NBTView.TagByteArray
}
final case class NBTString(value: String) extends NBTTag {
  override type Repr = String
  override type Self = NBTString
  override def nbtType: NBTType[String, Self] = NBTView.TagString
}

final case class NBTList[ElementRepr, ElementNBT <: NBTTag.Aux[ElementRepr]](
  value:                Seq[ElementNBT] with Seq[NBTTag.Aux[ElementRepr]] = Seq()
)(implicit val nbtType: NBTTypeInstances#NBTListType[ElementRepr, ElementNBT])
    extends NBTTag {

  override type Repr = Seq[ElementNBT]
  override type Self = NBTList[ElementRepr, ElementNBT]

  /**
		* Gets the [[NBTTag]] at the specified index.
		*/
  def apply(i: Int): ElementNBT = value(i)

  /**
		* Creates a new NBTList with this element prepended
		*/
  def +:(value: ElementNBT): NBTList[ElementRepr, ElementNBT] = NBTList(value +: this.value)

  /**
		* Creates a new NBTList with this element appended
		*/
  def :+(value: ElementNBT): NBTList[ElementRepr, ElementNBT] = NBTList(this.value :+ value)

  /**
		* Appends the specific [[NBTTag]]s to this [[NBTList]]
		*/
  def ++(values: ElementNBT*): NBTList[ElementRepr, ElementNBT] = NBTList(this.value ++ values)

  /**
		* The index of the specific element.
		*/
  def indexOf(obj: ElementNBT): Int = value.indexOf(obj)

  /**
		* The size of this list.
		*/
  def size: Int = value.size

  /**
		* If this list contains no elements
		*/
  def isEmpty: Boolean = value.isEmpty
}

final case class NBTCompound(value: Map[String, NBTTag] = Map()) extends NBTTag {
  override type Repr = Map[String, NBTTag]
  override type Self = NBTCompound
  override def nbtType: NBTType[Repr, Self] = NBTView.TagCompound

  /**
		* The size of this compound.
		*/
  def size: Int = value.size

  /**
		* Creates a new [[NBTCompound]] with the pair appended.
		*/
  def +(tuple: (String, NBTTag)): NBTCompound = NBTCompound(value + tuple)

  /**
    * Creates a new [[NBTCompound]] with the hlist appended. If there exists duplicate values. it uses the second one
    */
  def ++[Input <: HList, Mapped <: HList, Traversed](hList: Input)(
      implicit mapper: Mapper.Aux[NBTCompound.tupleToNBT.type, Input, Mapped],
      toTraversable: ToTraversable.Aux[Mapped, Seq, Traversed],
      evidence: Traversed <:< (String, NBTTag)): NBTCompound = this.merge(NBTCompound.fromHList(hList))

  /**
		* Creates a new [[NBTCompound]] with the key-value pair appended.
		*/
  def updated(key: String, tag: NBTTag): NBTCompound = NBTCompound(value.updated(key, tag))

  /**
		* Associates a specific tag to a specific key.
		*
		* @param key The key to bind to.
		* @param tag The tag to set.
		* @return An [[scala.Option]] with the previous value of the used key, or None if the key was not already used.
		*/
  def set(key: String, tag: NBTTag): NBTCompound = updated(key, tag)

  /**
		* Creates a NBTTag from the type passed in, and adds it to the compound.
		*
		* @param key The key to bind to.
		* @param value The value top set
		* @param to The converter to convert the value to a NBTTag
		* @tparam Repr The type to convert from
		* @tparam NBT The tag to convert to
		*/
  def setValue[Repr, NBT <: NBTTag](key: String, value: Repr)(implicit to: NBTView[Repr, NBT]): NBTCompound =
    set(key, to(value))

  /**
		* Creates two [[NBTLong]] tags from the UUID and sets the tags.
		*
		* This method differs in behavior from [[NBTViewInstances.UUIDView]].
		* If you want compatibility with vanilla, use this.
		*
		* The key of the two tags are key + "Most" for the most significant bits,
		* and key + "Least" for the least significant bits.
		*
		* @return The same things goes for this as for [[set]], only here you have a [[scala.collection.Seq]] instead of an [[scala.Option]]
		*/
  def setUUID(key: String, value: UUID): NBTCompound = {
    val most  = NBTLong(value.getMostSignificantBits)
    val least = NBTLong(value.getLeastSignificantBits)
    set(s"${key}Most", most).set(s"${key}Least", least)
  }

  /**
		* Tries to get a value in this [[NBTCompound]], or
		* throws an NoSuchElementException if no value is found.
		*/
  def apply(key: String): NBTTag = value(key)

  /**
		* Get a tag from this [[NBTCompound]]
		*/
  def get(key: String): Option[NBTTag] = value.get(key)

  /**
		* Gets a value from this if it exists at the specified key,
		* and it can be converted to the specified value.
		*/
  def getValue[Repr] = new NBTCompound.GetValue[Repr](this)

  /**
		* Tries to get an [[java.util.UUID]] created with [[setUUID]].
		*/
  def getUUID(key: String): Option[UUID] =
    get(s"${key}Most").collect {
      case NBTLong(most) =>
        get(s"${key}Least")
          .collect { case NBTLong(least) => new UUID(most, least) }
    }.flatten

  /**
		* Tries to get a [[NBTTag]] nested in multiple [[NBTCompound]].
		*
		* Example:
		*
		* {{{
		* val compound = NBTCompound().set("first" NBTCompound().set("second", NBTString("hi")))
		* assert(compound.getRecursive("first", "second") == NBTString("hi"))
		* }}}
		*/
  @tailrec
  def getRecursive(keys: String*): Option[NBTTag] = {
    val tail = keys.tail
    if (tail == Nil) get(keys.head)
    else
      get(keys.head) match {
        case Some(compound: NBTCompound) => compound.getRecursive(tail: _*)
        case _ => None
      }
  }

  /**
		* Same as [[getRecursive]], but with a value instead of a [[NBTTag]].
		*
		* @see [[getRecursive]]
		*/
  def getRecursiveValue[Repr] = new NBTCompound.getRecursiveValue[Repr](this)

  /**
		* Tries to merge this [[NBTCompound]] with another.
		* If a situation where both compounds contain some value with the same key arises,
		* the merge function is used.
		*/
  def mergeAdvanced(other: NBTCompound)(merge: ((String, NBTTag), (String, NBTTag)) => (String, NBTTag)): NBTCompound = {
    val conflictKeys = value.keySet.intersect(other.value.keySet)

    def mergePot(first: NBTTag, second: NBTTag): Option[NBTTag] =
      first match {
        case thisCompound: NBTCompound =>
          second match {
            case thatCompound: NBTCompound =>
              Some(thisCompound.mergeAdvanced(thatCompound)(merge))
            case _ => None
          }
        case _ => None
      }

    def handleConflict(conflicted: (String, NBTTag), others: Seq[(String, NBTTag)]): ((String, NBTTag), Seq[(String, NBTTag)]) = {
      val otherKV        = others.find(kv => kv._1 == conflicted._1).get //Get is completely safe here as we already know that both sequences contains the value
      val filteredOthers = others.filter(kv => kv != otherKV)

      mergePot(conflicted._2, otherKV._2) match {
        case Some(merged) => ((otherKV._1, merged), filteredOthers)
        case None         => (merge(conflicted, otherKV), filteredOthers)
      }
    }

    @tailrec
    def inner(thisRest: Seq[(String, NBTTag)], thatRest: Seq[(String, NBTTag)], acc: Map[String, NBTTag]): Map[String, NBTTag] =
      if (thisRest.isEmpty) acc ++ thatRest
      else if (thatRest.isEmpty) acc ++ thisRest
      else {
        val thisHead @ (thisName, _) = thisRest.head
        val thatHead @ (thatName, _) = thatRest.head

        if (conflictKeys.contains(thisName)) {
          val (merged, newThat) = handleConflict(thisHead, thatRest)
          inner(thisRest.tail, newThat, acc + merged)
        } else if (conflictKeys.contains(thatName)) {
          val (merged, newThis) = handleConflict(thatHead, thisRest)
          inner(thisRest.tail, newThis, acc + merged)
        } else inner(thisRest.tail, thatRest.tail, acc + thisHead + thatHead)
      }

    NBTCompound(inner(value.toSeq, other.value.toSeq, Map()))
  }

  /**
		* Merges this [[NBTCompound]] with another, and if a conflict arises, uses the second one.
		*/
  def merge(other: NBTCompound): NBTCompound = mergeAdvanced(other)((first, second) => second)

  /**
		* Checks if this [[NBTCompound]] has a specific key.
		*/
  def hasKey(key: String): Boolean = value.contains(key)
}
object NBTCompound {

  def apply[Repr, NBT <: NBTTag](map: Map[String, Repr])(implicit view: NBTView[Repr, NBT]): NBTCompound =
    new NBTCompound(map.mapValues(view.apply))

  object tupleToNBT extends Poly1 {
    implicit def apply[Repr, NBT <: NBTTag](implicit view: NBTView[Repr, NBT]) =
      at[(String, Repr)] { case (name, value) => name -> view(value) }
  }

  def fromHList[Input <: HList, Mapped <: HList, Traversed](elements: Input)(
      implicit mapper: Mapper.Aux[tupleToNBT.type, Input, Mapped],
      toTraversable: ToTraversable.Aux[Mapped, Seq, Traversed],
      evidence: Traversed <:< (String, NBTTag)) =
    NBTCompound(elements.map(tupleToNBT).to[Seq].toMap)

  class GetValue[Repr](compound: NBTCompound) {
    def apply[NBT <: NBTTag](key: String)(implicit view: NBTView[Repr, NBT], tpe: Typeable[NBT]): Option[Repr] = {
      compound.get(key).flatMap(nbt => tpe.cast(nbt).flatMap(view.unapply))
    }
  }

  class getRecursiveValue[Repr](nbt: NBTCompound) {
    def apply[NBT <: NBTTag](keys: String*)(implicit from: NBTView[Repr, NBT], tpe: Typeable[NBT]): Option[Repr] = {
      val tail = keys.tail
      if (tail == Nil) nbt.getValue[Repr](keys.head)
      else
        nbt.get(keys.head) match {
          case Some(compound: NBTCompound) => compound.getRecursiveValue[Repr](tail: _*)
          case _ => None
        }
    }
  }
}

final case class NBTIntArray(value: IndexedSeq[Int]) extends NBTTag {
  override type Repr = IndexedSeq[Int]
  override type Self = NBTIntArray
  override def nbtType: NBTType[Repr, Self] = NBTView.TagIntArray
}
final case class NBTLongArray(value: IndexedSeq[Long]) extends NBTTag {
  override type Self = NBTLongArray
  override type Repr = IndexedSeq[Long]
  override def nbtType: NBTType[IndexedSeq[Long], NBTLongArray] = NBTView.TagLongArray
}