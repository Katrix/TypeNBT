package net.katsstuff.typenbt.derivation

import scala.reflect.macros.blackbox

import net.katsstuff.typenbt._

class NBTDerivation(val c: blackbox.Context) {
  import c.universe._

  val compoundTpe: Type = typeOf[NBTCompound]

  private val nbtTypeMap = Map(
    typeOf[Byte]             -> typeOf[NBTByte],
    typeOf[Short]            -> typeOf[NBTShort],
    typeOf[Int]              -> typeOf[NBTInt],
    typeOf[Long]             -> typeOf[NBTLong],
    typeOf[Float]            -> typeOf[NBTFloat],
    typeOf[Double]           -> typeOf[NBTDouble],
    typeOf[IndexedSeq[Byte]] -> typeOf[NBTByteArray],
    typeOf[String]           -> typeOf[NBTString],
    //typeOf[Seq] -> typeOf[NBTList],
    typeOf[Map[String, _]]   -> compoundTpe,
    typeOf[IndexedSeq[Int]]  -> typeOf[NBTIntArray],
    typeOf[IndexedSeq[Long]] -> typeOf[NBTLongArray]
  )

  def createView[A: WeakTypeTag]: Expr[NBTView[A, NBTCompound]] = {
    val tpe = weakTypeOf[A]

    if (!tpe.typeSymbol.isClass) {
      c.abort(c.enclosingPosition, s"$tpe is not a class or trait")
    }

    if (nbtTypeMap.values.exists(t => tpe =:= t)) {
      c.abort(c.enclosingPosition, s"$tpe is an NBT type, you can't create a new view for that")
    }

    val classSymbol = tpe.typeSymbol.asClass

    if (classSymbol.isCaseClass) {
      deriveCaseClass(tpe)
    } else if (classSymbol.isTrait && classSymbol.isSealed) {
      val subclasses = classSymbol.knownDirectSubclasses
      if (subclasses.isEmpty) {
        c.abort(c.enclosingPosition, s"No known subclasses for $tpe")
      }

      deriveSealedTrait(tpe, subclasses)
    } else c.abort(c.enclosingPosition, s"$tpe is not a case class or sealed trait")
  }

  def deriveCaseClass[A](tpe: Type): Expr[NBTView[A, NBTCompound]] = {
    val optPrimaryCtor = tpe.decls
      .collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }

    if (optPrimaryCtor.isEmpty) {
      c.abort(c.enclosingPosition, s"No primary constructor found for $tpe")
    }
    val primaryCtor = optPrimaryCtor.get

    val fields = primaryCtor.paramLists.head

    if (fields.isEmpty) {
      c.abort(c.enclosingPosition, s"$tpe has a no arg primary constructor")
    }

    val fieldsToNamesAndCode = fields.map { field =>
      val fieldType        = field.typeSignatureIn(tpe)
      val fieldName        = field.name.toTermName.decodedName.toString
      val serializerName   = TermName(c.freshName(fieldName + "Serializer"))
      val deserializerName = TermName(c.freshName(fieldName + "Deserializer"))
      val refinerName      = TermName(c.freshName(fieldName + "Refiner"))
      val code = Seq(
        q"""private val $serializerName = _root_.net.katsstuff.typenbt.NBTSerializer.forRepr[$fieldType].find""",
        q"""private val $deserializerName = _root_.net.katsstuff.typenbt.NBTDeserializer.forRepr[$fieldType].find""",
        q"""private val $refinerName = _root_.net.katsstuff.typenbt.NBTRefiner.fromDeserializer($deserializerName)"""
      )

      (field, (code, serializerName, deserializerName, refinerName))
    }.toMap

    val codeVals = fieldsToNamesAndCode.values.map(_._1).toList.flatten

    val (toCompound, fromCompound) = fieldsToNamesAndCode.map {
      case (field, (_, serializerName, deserializerName, refinerName)) =>
        val name = field.name.toTermName
        val key  = name.decodedName.toString

        (
          q"($key, $serializerName.to(v.$name))",
          fq"$name <- arg.get($key).flatMap($refinerName.refine).flatMap($deserializerName.from)"
        )
    }.unzip

    val resTree = q"""
         new _root_.net.katsstuff.typenbt.NBTView[$tpe, _root_.net.katsstuff.typenbt.NBTCompound] {
             ..$codeVals

           override def to(v: $tpe): _root_.net.katsstuff.typenbt.NBTCompound = {
             _root_.net.katsstuff.typenbt.NBTCompound(_root_.scala.Predef.Map(..$toCompound))
           }

           override def from(arg: _root_.net.katsstuff.typenbt.NBTCompound): _root_.scala.Option[$tpe] =
             for(..$fromCompound) yield new $tpe(..${fields.map(_.name.toTermName)})
         }
       """

    val res = c.Expr[NBTView[A, NBTCompound]](resTree)

    //c.info(c.enclosingPosition, showCode(resTree), force = false)

    res
  }

  def deriveSealedTrait[A](tpe: Type, subClasses: Set[Symbol]): Expr[NBTView[A, NBTCompound]] = {
    val subClassesToNamesAndCode = subClasses.map { subClass =>
      val fieldType        = subClass.asType
      val fieldName        = subClass.name.toTermName.decodedName.toString
      val serializerName   = TermName(c.freshName(fieldName + "Serializer"))
      val deserializerName = TermName(c.freshName(fieldName + "Deserializer"))
      val refinerName      = TermName(c.freshName(fieldName + "Refiner"))

      val code = Seq(
        q"""private val $serializerName = _root_.net.katsstuff.typenbt.NBTSerializer.forRepr[$fieldType].find""",
        q"""private val $deserializerName = _root_.net.katsstuff.typenbt.NBTDeserializer.forRepr[$fieldType].find""",
        q"""private val $refinerName = _root_.net.katsstuff.typenbt.NBTRefiner.fromDeserializer($deserializerName)"""
      )

      (subClass, (code, serializerName, deserializerName, refinerName))
    }.toMap

    val codeVals = subClassesToNamesAndCode.values.map(_._1).toList.flatten

    val (toCompound, fromCompound) = subClassesToNamesAndCode.map {
      case (subClass, (_, serializerName, deserializerName, refinerName)) =>
        val subClassTpe = subClass.asType
        val typeName    = subClass.name.toTermName.decodedName.toString

        (
          cq"""value: $subClassTpe => _root_.scala.Predef.Map(("$$type", _root_.net.katsstuff.typenbt.NBTString($typeName)), ("$$value", $serializerName.to(value)))""",
          cq"""_root_.net.katsstuff.typenbt.NBTString($typeName) => arg.get("$$value").flatMap($refinerName.refine).flatMap($deserializerName.from)"""
        )
    }.unzip

    val fromPartial = q"{ case ..$fromCompound }"

    val resTree = q"""
         new _root_.net.katsstuff.typenbt.NBTView[$tpe, _root_.net.katsstuff.typenbt.NBTCompound] {
             ..$codeVals

           override def to(v: $tpe): _root_.net.katsstuff.typenbt.NBTCompound = {
             val nbtMap = v match {
               case ..$toCompound
             }
             _root_.net.katsstuff.typenbt.NBTCompound(nbtMap)
           }

           override def from(arg: _root_.net.katsstuff.typenbt.NBTCompound): Option[$tpe] = arg.get("$$type").collect {
             case ..$fromCompound
           }.flatten
         }
       """

    val res = c.Expr[NBTView[A, NBTCompound]](resTree)

    //c.info(c.enclosingPosition, showCode(resTree), force = false)

    res
  }
}
