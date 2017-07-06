# TypeNBT

TypeNBT is a idiomatic, type safe NBT library for Scala. TypeNBT allows you to focus on the data in the code, not the NBT as most other libraries requires. TypeNBT is in active development.

Add TypeNBT to your project by including this line in build.sbt
```scala
libraryDependecies += "net.katsstuff" %% "typenbt" % "0.2"
//Or this if you use Scala.js
libraryDependecies += "net.katsstuff" %%% "typenbt" % "0.2"
```

## Why TypeNBT?
Why did I decide to write TypeNBT and not just use something that already existed.
* You can easily include TypeNBT in your build find
* Everything in TypeNBT is immutable, this includes the collections
* TypeNBT is type safe. There are no surprises when running the code and getting an exception back because you expected the wrong data type. Even the list is type safe.
* It includes the type information at runtime. How do serialize an empty list in other libraries, you can't really, but here you can do that.
* Convert anything to NBT. Other libraries also has methods to convert a value to a tag, but TypeNBT let's you define implicit objects to define how this conversion should happen. This aspect of the library isn't hidden away either. Instead it's a code part of how it works.
* Use of things like `Try` and `Option`
* Create a NBTCompound easily. TypeNBT easily allows you to create your entire NBTCompound in a single line using a HList like this `NBTCompound.fromHList("first" -> "hi" :: "second" -> 5 :: "third" -> false :: HNil)`
* Easy conversion to and from common types not represented by raw NBT. Notice the boolean in the above line.
* Full support for Mojangson parsing.
* TypeNBT works for Scala.js
* TypeNBT is updated and actively being worked on.

## Using TypeNBT

Here is some informatio about how to use TypeNBT in practice.
For all of these, make sure you import `net.katsstuff.typenbt._`.

## Creating NBTTag, and about NBTView and NBTType

Converting a value to nbt can be done like this:
```scala
import net.katsstuff.typenbt._
5.nbt //Int, return NBTInt
"hi".nbt //String, returns NBTString
false.nbt //Boolean, returns NBTByte
IndexedSeq(2, 5).nbt //IndexedSeq[Int], returns NBTIntArray
NBTInt(1) //You can also create the NBTTag more explicitly
```

Behind the scenes this uses an implicit object called NBTView. NBTView takes two type members, the target(NBT), and the origin(Repr). NBTView has two methods, `apply` to construct an `NBT` from `Repr`, and `unapply` to deconstruct an `NBT` to `Repr`. If you whish to easily convert a data structure to nbt, all you have to do is to write an implicit NBTView for your datatype, and make sure it's in scope. (Derivation of NBTView is currently broken for case classes.)

There are also special versions of these called NBTType. These are more strict in that the nbt type member must point to the repr member. They also include the id of the tag for use when serializing a tag. All `NBTTag`s also has a member called `nbtType` which points to it's respective type.

## Creating a NBTCompound from a HList

TypeNBT allows you to convert a `HList` into a `NBTCompound` or add a `HList` to an existing `NBTCompound` rather easily. First make sure you have your HList. The HList must consist of tuples from string to values that a NBTView exists for. Second call `NBTCompound.fromHList(hList)` or `compound ++ hList`. TypeNBT takes care of the rest.

## On multiple type parameter lists

For some methods like `NBTCompount#getValue`, TypeNBT uses multiple parameter lists in the form of anonymous classes. Unless you really want to, you generally only have to fill in one of them.

## Mojangson
TypeNBT includes a Mojangson parser that let's you easily both parse mojangson, as well as convert to mojangson. Simply use `Mojangson.toMojangson` and `Mojangson.fromMojangson`
