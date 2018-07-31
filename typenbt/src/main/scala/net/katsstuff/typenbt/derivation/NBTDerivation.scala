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
    typeOf[Map[String, _]]  -> compoundTpe,
    typeOf[IndexedSeq[Int]] -> typeOf[NBTIntArray]
  )

  def createView[A: WeakTypeTag]: Expr[NBTView[A, NBTCompound]] = {
    val tpe = weakTypeOf[A]

    if (nbtTypeMap.values.exists(t => tpe =:= t)) {
      c.abort(c.enclosingPosition, s"$tpe is an NBT type, you can't create a new view for that")
    }

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
      val serializerName   = TermName(c.freshName(fieldName + "serializer"))
      val deserializerName = TermName(c.freshName(fieldName + "deserializer"))
      val refinerName      = TermName(c.freshName(fieldName + "refiner"))
      val code = Seq(
        q"""val $serializerName = _root_.net.katsstuff.typenbt.NBTSerializer.forRepr[$fieldType].find""",
        q"""val $deserializerName = _root_.net.katsstuff.typenbt.NBTDeserializer.forRepr[$fieldType].find""",
        q"""val $refinerName = _root_.net.katsstuff.typenbt.NBTRefiner.fromDeserializer($deserializerName)"""
      )

      (field, (code, serializerName, deserializerName, refinerName))
    }.toMap

    val viewVals = fieldsToNamesAndCode.values.map(_._1).toList.flatten

    val (toCompound, fromCompound) = fieldsToNamesAndCode.map {
      case (field, (_, serializerName, deserializerName, refinerName)) =>
        val name = field.name.toTermName
        val key  = name.decodedName.toString

        (
          q"$key -> $serializerName.to(v.$name)",
          fq"$name <- arg.get($key).flatMap($refinerName.refine).flatMap($deserializerName.from)"
        )
    }.unzip

    val resTree = q"""
         new _root_.net.katsstuff.typenbt.NBTView[$tpe, _root_.net.katsstuff.typenbt.NBTCompound] {
             ..$viewVals

           override def to(v: $tpe): _root_.net.katsstuff.typenbt.NBTCompound = {
             _root_.net.katsstuff.typenbt.NBTCompound(Map(..$toCompound))
           }

           override def from(arg: _root_.net.katsstuff.typenbt.NBTCompound): Option[$tpe] =
             for(..$fromCompound) yield new $tpe(..${fields.map(_.name.toTermName)})
         }
       """

    val res = c.Expr[NBTView[A, NBTCompound]](resTree)

    c.echo(c.enclosingPosition, resTree.toString())

    res
  }

  def createViewOld[A: WeakTypeTag]: Expr[NBTView[A, NBTCompound]] = {
    val tpe       = weakTypeOf[A]
    val companion = tpe.typeSymbol.companion

    val fields = tpe.decls
      .collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }
      .get
      .paramLists
      .head

    val fieldsToNbt = fields.map { field =>
      val fieldType = field.typeSignature
      val optNbt    = nbtTypeMap.get(fieldType)
      field -> optNbt.getOrElse(typeOf[NBTCompound])
    }.toMap

    val (toCompound, fromCompound) = fields.map { field =>
      val nbt           = fieldsToNbt(field)
      val name          = field.asTerm.name
      val typeSignature = field.typeSignature
      val key           = name.decodedName.toString

      (
        q"$key -> io.github.katrix.typenbt.nbt.NBTView[$typeSignature, $nbt].apply(v.$name)",
        q"nbt.getValue[$nbt, $typeSignature]($key)"
      )
    }.unzip

    val res = c.Expr[NBTView[A, NBTCompound]] {
      q"""
        new io.github.katrix.typenbt.nbt.NBTView[$tpe, io.github.katrix.typenbt.nbt.NBTCompound] {
          override implicit def apply(v: $tpe): io.github.katrix.typenbt.nbt.NBTCompound = io.github.katrix.typenbt.nbt.NBTCompound(Map(..$toCompound))
          override implicit def unapply(nbt: io.github.katrix.typenbt.nbt.NBTCompound): Option[$tpe] = {
            val valid = !Seq(..$fromCompound).contains(None)
            if(valid) Some($companion(..${fromCompound.map(t => q"$t.get")})) else None
          }
        }
     """
    }

    //c.info(c.enclosingPosition, res.toString(), force = true)

    res
  }
}
