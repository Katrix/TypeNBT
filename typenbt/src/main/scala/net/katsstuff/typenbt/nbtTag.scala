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

import java.util.UUID

import scala.annotation.tailrec

sealed trait NBTTag {

  /** The value that this [[NBTTag]] holds. */
  type Repr

  /** The value of this [[NBTTag]] */
  def value: Repr

  /** The type of this [[NBTTag]] */
  def nbtType: NBTType.CovarObj[Repr]

  /**
    * Modifies this NBT in value form before returning a new NBT. Thew two types
    * of NBT does not have to be the same.
    *
    * @example
    *   {{{val stringNbt: NBTString = NBTInt(5).modify(_.toString)}}}
    * @param f
    *   The function to apply to the NBT
    * @param serializer
    *   A view providing a way to get back to the world of NBTs after the
    *   modification.
    * @tparam NewRepr
    *   The new value type
    * @tparam NewNBT
    *   The new NBT type
    */
  def modify[NewRepr, NewNBT <: NBTTag](
      f: Repr => NewRepr
  )(implicit serializer: NBTSerializer[NewRepr, NewNBT]): NewNBT =
    serializer.to(f(value))
}

object NBTTag {
  type Aux[Repr0] = NBTTag { type Repr = Repr0 }
}

/**
  * The NBTEnd type. There are no actual values of this floating around. Think
  * of it in the same way you think about Nothing.
  */
sealed trait NBTEnd extends NBTTag {
  override type Repr = Nothing
  override def nbtType: NBTType[Repr, NBTEnd] = NBTType.TagEnd
}
final case class NBTByte(value: Byte) extends NBTTag {
  override type Repr = Byte
  override def nbtType: NBTType[Repr, NBTByte] = NBTType.TagByte
}
final case class NBTShort(value: Short) extends NBTTag {
  override type Repr = Short
  override def nbtType: NBTType[Repr, NBTShort] = NBTType.TagShort
}
final case class NBTInt(value: Int) extends NBTTag {
  override type Repr = Int
  override def nbtType: NBTType[Repr, NBTInt] = NBTType.TagInt
}
final case class NBTLong(value: Long) extends NBTTag {
  override type Repr = Long
  override def nbtType: NBTType[Repr, NBTLong] = NBTType.TagLong
}
final case class NBTFloat(value: Float) extends NBTTag {
  override type Repr = Float
  override def nbtType: NBTType[Repr, NBTFloat] = NBTType.TagFloat
}
final case class NBTDouble(value: Double) extends NBTTag {
  override type Repr = Double
  override def nbtType: NBTType[Repr, NBTDouble] = NBTType.TagDouble
}
final case class NBTByteArray(value: IndexedSeq[Byte]) extends NBTTag {
  override type Repr = IndexedSeq[Byte]
  override def nbtType: NBTType[Repr, NBTByteArray] = NBTType.TagByteArray
}
final case class NBTString(value: String) extends NBTTag {
  override type Repr = String
  override def nbtType: NBTType[String, NBTString] = NBTType.TagString
}

final case class NBTList[ElementRepr, ElementNBT <: NBTTag.Aux[ElementRepr]](
    value: Seq[ElementNBT] with Seq[NBTTag.Aux[ElementRepr]] =
      Seq() // The with here is used to help the compiler infer the correct type
)(implicit val nbtType: NBTListType[ElementRepr, ElementNBT])
    extends NBTTag {

  override type Repr = Seq[ElementNBT]

  /** Gets the [[NBTTag]] at the specified index. */
  def apply(i: Int): ElementNBT = value(i)

  /** Set the specified element at the specified position */
  def updated(i: Int, value: ElementNBT): NBTList[ElementRepr, ElementNBT] = NBTList(this.value.updated(i, value))

  /** Creates a new NBTList with this element prepended */
  def +:(value: ElementNBT): NBTList[ElementRepr, ElementNBT] = NBTList(value +: this.value)

  /** Creates a new NBTList with this element appended */
  def :+(value: ElementNBT): NBTList[ElementRepr, ElementNBT] = NBTList(this.value :+ value)

  /** Appends the specific [[NBTTag]]s to this [[NBTList]] */
  def ++(values: ElementNBT*): NBTList[ElementRepr, ElementNBT] = NBTList(this.value ++ values)

  /** The index of the specific element. */
  def indexOf(obj: ElementNBT): Int = value.indexOf(obj)

  /** The size of this list. */
  def size: Int = value.size

  /** If this list contains no elements */
  def isEmpty: Boolean = value.isEmpty
}

final case class NBTCompound(value: Map[String, NBTTag] = Map()) extends NBTTag {
  import NBTCompound.NamedTag

  override type Repr = Map[String, NBTTag]
  override def nbtType: NBTType[Repr, NBTCompound] = NBTType.TagCompound

  /** The size of this compound. */
  def size: Int = value.size

  /** Creates a new [[NBTCompound]] with the pair appended. */
  def +(tuple: NamedTag): NBTCompound = NBTCompound(value + tuple)

  /** Creates a new [[NBTCompound]] with the key-value pair appended. */
  def update(key: String, tag: NBTTag): NBTCompound = NBTCompound(value.updated(key, tag))

  /**
    * Associates a specific tag to a specific key.
    *
    * @param key
    *   The key to bind to.
    * @param tag
    *   The tag to set.
    */
  def set(key: String, tag: NBTTag): NBTCompound = update(key, tag)

  /**
    * Creates a NBTTag from the type passed in, and adds it to the compound.
    *
    * @param key
    *   The key to bind to.
    * @param value
    *   The value top set
    * @param serializer
    *   The converter to convert the value to a NBTTag
    * @tparam ValueRepr
    *   The type to convert from
    * @tparam NBT
    *   The tag to convert to
    */
  def setValue[ValueRepr, NBT <: NBTTag](key: String, value: ValueRepr)(
      implicit serializer: NBTSerializer[ValueRepr, NBT]
  ): NBTCompound =
    set(key, serializer.to(value))

  /**
    * Creates two [[NBTLong]] tags from the UUID and sets the tags.
    *
    * This method differs in behavior from [[NBTViewInstances.UUIDView]]. If you
    * want compatibility with vanilla, use this.
    *
    * The keys of the two tags are key + "Most" for the most significant bits,
    * and key + "Least" for the least significant bits.
    */
  def setUUID(key: String, value: UUID): NBTCompound = {
    val most  = NBTLong(value.getMostSignificantBits)
    val least = NBTLong(value.getLeastSignificantBits)
    set(s"${key}Most", most).set(s"${key}Least", least)
  }

  /**
    * Tries to get a value in this [[NBTCompound]], or throws an
    * NoSuchElementException if no value is found.
    */
  def apply(key: String): NBTTag = value(key)

  /** Get a tag from this [[NBTCompound]] */
  def get(key: String): Option[NBTTag] = value.get(key)

  /** Tries to get an [[java.util.UUID]] created with [[setUUID]]. */
  def getUUID(key: String): Option[UUID] =
    get(s"${key}Most").collect { case NBTLong(most) =>
      get(s"${key}Least").collect { case NBTLong(least) => new UUID(most, least) }
    }.flatten

  /**
    * Tries to get a [[NBTTag]] nested in multiple [[NBTCompound]].
    *
    * Example:
    *
    * {{{
    * val compound = NBTCompound().set("first" NBTCompound().set("second", NBTString("hi")))
    * assert(compound.getNested("first", "second") == NBTString("hi"))
    * }}}
    */
  @tailrec
  def getNested(keys: String*): Option[NBTTag] = {
    val tail = keys.tail
    if (tail == Nil) get(keys.head)
    else
      get(keys.head) match {
        case Some(compound: NBTCompound) => compound.getNested(tail: _*)
        case _                           => None
      }
  }

  /**
    * Tries to merge this [[NBTCompound]] with another. If a situation where
    * both compounds contain some value with the same key arises, the merge
    * function is used.
    */
  def mergeAdvanced(other: NBTCompound)(merge: (NamedTag, NamedTag) => NamedTag): NBTCompound = {
    val conflictKeys = value.keySet.intersect(other.value.keySet)

    def handleConflict(firstKV: NamedTag, rest: Seq[NamedTag]): (NamedTag, Seq[NamedTag]) = {
      val otherKV =
        rest
          .find(kv => kv._1 == firstKV._1)
          .get // Get is completely safe here as we already know that both sequences contains the value
      val newRest = rest.filter(kv => kv != otherKV)

      firstKV._2 match {
        case firstCompound: NBTCompound =>
          otherKV._2 match {
            case secondCompound: NBTCompound =>
              ((otherKV._1, firstCompound.mergeAdvanced(secondCompound)(merge)), newRest)
            case _ => (merge(firstKV, otherKV), newRest)
          }

        case _ => (merge(firstKV, otherKV), newRest)
      }
    }

    @tailrec
    def inner(thisRest: Seq[NamedTag], thatRest: Seq[NamedTag], acc: Map[String, NBTTag]): Map[String, NBTTag] =
      if (thisRest.isEmpty) acc ++ thatRest
      else if (thatRest.isEmpty) acc ++ thisRest
      else {
        val thisHead @ (thisName, _) = thisRest.head
        val thatHead @ (thatName, _) = thatRest.head

        if (conflictKeys.contains(thisName)) {
          val (merged, newThat) = handleConflict(thisHead, thatRest)
          inner(thisRest.tail, newThat, acc + merged)
        } else if (conflictKeys.contains(thatName)) {
          val (merged, newThat) = handleConflict(thatHead, thisRest)
          inner(thisRest.tail, newThat, acc + merged)
        } else inner(thisRest.tail, thatRest.tail, acc + thisHead + thatHead)
      }

    NBTCompound(inner(value.toSeq, other.value.toSeq, Map.empty))
  }

  /**
    * Gets a value from this if it exists at the specified key, and it can be
    * converted to the specified value.
    */
  def getValue[ValueRepr]: NBTCompound.GetValueNBTCompound[ValueRepr] =
    new NBTCompound.GetValueNBTCompound[ValueRepr](this)

  /**
    * Same as [[NBTCompound.getNested]], but with a value instead of a
    * [[NBTTag]].
    *
    * @see
    *   [[NBTCompound.getNested]]
    */
  def getNestedValue[ValueRepr]: NBTCompound.GetRecursiveValueNBTCompound[ValueRepr] =
    new NBTCompound.GetRecursiveValueNBTCompound[ValueRepr](this)

  /**
    * Merges this [[NBTCompound]] with another, and if a conflict arises, uses
    * the second one.
    */
  def merge(other: NBTCompound): NBTCompound = mergeAdvanced(other)((_, second) => second)

  /** Checks if this [[NBTCompound]] has a specific key. */
  def hasKey(key: String): Boolean = value.contains(key)
}
object NBTCompound {

  type NamedTag = (String, NBTTag)

  def apply(values: (String, NBTTag)*): NBTCompound = NBTCompound(values.toMap)

  def apply[Repr, NBT <: NBTTag](map: Map[String, Repr])(implicit serializer: NBTSerializer[Repr, NBT]): NBTCompound =
    new NBTCompound(map.map(t => t._1 -> serializer.to(t._2)))

  class GetValueNBTCompound[Repr](private val compound: NBTCompound) extends AnyVal {
    def apply[NBT <: NBTTag](
        key: String
    )(implicit deserializer: NBTDeserializer[Repr, NBT], refiner: NBTRefiner[NBT]): Option[Repr] =
      compound.get(key).flatMap(nbt => refiner.refine(nbt).flatMap(deserializer.from))
  }

  class GetRecursiveValueNBTCompound[Repr](private val compound: NBTCompound) extends AnyVal {
    def apply[NBT <: NBTTag](
        keys: String*
    )(implicit deserializer: NBTDeserializer[Repr, NBT], refiner: NBTRefiner[NBT]): Option[Repr] = {
      val tail = keys.tail
      if (tail == Nil) compound.getValue[Repr](keys.head)
      else
        compound.get(keys.head) match {
          case Some(compound: NBTCompound) => compound.getNestedValue[Repr](tail: _*)
          case _                           => None
        }
    }
  }
}

final case class NBTIntArray(value: IndexedSeq[Int]) extends NBTTag {
  override type Repr = IndexedSeq[Int]
  override def nbtType: NBTType[Repr, NBTIntArray] = NBTType.TagIntArray
}
final case class NBTLongArray(value: IndexedSeq[Long]) extends NBTTag {
  override type Repr = IndexedSeq[Long]
  override def nbtType: NBTType[IndexedSeq[Long], NBTLongArray] = NBTType.TagLongArray
}
