name := "Type NBT"
organization := "io.github.katrix"
version := "0.1"

scalaVersion := "2.12.0"

scalacOptions += "-Ypartial-unification" // enable fix for SI-2712

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
