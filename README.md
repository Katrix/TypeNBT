# TypeNBT

TypeNBT is a idiomatic, type safe NBT library for Scala. TypeNBT allows you to focus on the data in the code, not the NBT as most other libraries requires.

Add TypeNBT to your project by including this line in build.sbt
```scala
libraryDependencies += "net.katsstuff" %% "typenbt" % "0.4"
//Or this if you use Scala.js
libraryDependencies += "net.katsstuff" %%% "typenbt" % "0.4"
```

## Why TypeNBT?
Why did I decide to write TypeNBT and not just use something that already existed.
* You can find TypeNBT on maven
* Everything in TypeNBT is immutable, this includes the collections
* TypeNBT is type safe. There are no surprises when running the code and getting an exception back because you expected the wrong data type. Even the list is type safe
* It includes the type information at runtime. How do serialize an empty list in other libraries, you can't really, but here you can do that
* Convert anything to NBT. TypeNBT defines typeclasses for encoding and decoding a value to NBT. This aspect of the library isn't hidden away either. Instead it's a core part of how it works
* Create a NBTCompound easily. TypeNBT easily allows you to create your entire NBTCompound in a single line using a HList like this `NBTCompound.fromHList("first" -> "hi" :: "second" -> 5 :: "third" -> false :: HNil)`. *NOTE*: Requires the use of the `typenbt-extra` module
* Easy conversion to and from common types not represented by raw NBT. Notice the boolean in the above line
* Full support for Mojangson parsing. *NOTE*: Requires the `typenbt-mojangson` module.
* TypeNBT works for Scala.js

## Using TypeNBT

Here is some information about how to use TypeNBT in practice.
For all of these, make sure you import `net.katsstuff.typenbt._`.

## Creating NBTTag, and the typeclasses that TypeNBT uses

Converting a value to nbt can be done like this:
```scala
import net.katsstuff.typenbt._
5.nbt //Int, return NBTInt
"hi".nbt //String, returns NBTString
false.nbt //Boolean, returns NBTByte
IndexedSeq(2, 5).nbt //IndexedSeq[Int], returns NBTIntArray
NBTInt(1) //You can also create the NBTTag more explicitly
```

### NBTSerializer
This uses the typeclass `NBTSerializer[Repr, NBT]` which takes to types, the type to convert from, and the type to convert to. This is analogous to the type `Repr => NBT`.

### NBTDeserializer
There is also `NBTDeserializer[Repr, NBT]` which goes the other way around, except that it returns an `Option` as the data might not be valid for a given type. This is analogous to the type `NBT => Option[Repr]`.

### SafeNBTDeserializer
For cases where a value can always be safely converted from an nbt value, there exists `SafeNBTDeserializer`. This is analogous to the type `NBT => Repr`.

### NBTView
Next there is `NBTView` which combines `NBTSerializer` and `NBTDeserializer`. 

### SafeNBTView
There is also `SafeNBTView` which uses `SafeNBTDeserializer` instead of `NBTDeserializer`. 

### CaseLike
Then there is `NBTViewCaseLike` and `SafeNBTViewCaseLike`. Which adds apply and unapply methods to the view to make certain types behave like they are normal nbt types. For example, you can do `NBTBoolean(false)` which will then convert the boolean to an nbt byte. 

### NBTType
Lastly there is `NBTType` which corresponds to the base nbt types. This also contains the byte id for the type.

## On multiple type parameter lists

For some methods like `NBTCompount#getValue`, TypeNBT uses multiple parameter lists in the form of anonymous classes. Unless you really want to, you generally only have to fill in one of them.

## typenbt-extra
If you want more fanciness, then there is also the module `typenbt-extra`, which contains some more operations which uses shapeless under the hood.

First add the dependency to your build.
```scala
libraryDependencies += "net.katsstuff" %% "typenbt-extra" % "0.4"
//Or this if you use Scala.js
libraryDependencies += "net.katsstuff" %%% "typenbt-extra" % "0.4"
```

Now you can convert a `HList` into a `NBTCompound` or add a `HList` to an existing `NBTCompound`. First make sure you have your HList. The HList must consist of tuples from string to values that an NBTSerializer exists for. Then import `net.katsstuff.typenbt.extra._` and call `NBTCompound.fromHList(hList)` or do `compound ++ hList`. TypeNBT takes care of the rest.

## typenbt-mojangson
TypeNBT also has another module for both parsing and creating mojangson.

```scala
libraryDependencies += "net.katsstuff" %% "typenbt-mojangson" % "0.4"
//Or this if you use Scala.js
libraryDependencies += "net.katsstuff" %%% "typenbt-mojangson" % "0.4"
```

You can then use `Mojangson.toMojangson` and `Mojangson.fromMojangson`

## Examples
There exists more examples on how to use TypeNBT in the examples directory.