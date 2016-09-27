name := "Type NBT"
organization := "io.github.katrix"
version := "0.1"

scalaVersion := "2.11.8"

scalacOptions += "-Xexperimental"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % Test