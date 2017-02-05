lazy val commonSettings = Seq(
  scalaVersion := "2.12.0",
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.4.2",
  libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.2"
)

lazy val rootTypeNBT = project
  .in(file("."))
  .aggregate(typenbtJVM, typenbtJS)
  .settings(
    organization := "net.katsstuff",
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    crossScalaVersions := Seq("2.11.8", "2.12.0")
  )

lazy val typenbt = crossProject
  .crossType(CrossType.Pure)
  .settings(commonSettings: _*)
  .settings(
    name := "TypeNBT",
    organization := "net.katsstuff",
    version := "0.2",
    libraryDependencies += "org.scalactic"  %%% "scalactic"  % "3.0.0"  % Test,
    libraryDependencies += "org.scalatest"  %%% "scalatest"  % "3.0.0"  % Test,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ =>
      false
    },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    description := "TypeNBT is a NBT library that let's the user focus on the data, not how it's represented",
    licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
    scmInfo := Some(
      ScmInfo(url("https://github.com/Katrix-/TypeNBT"), "scm:git:github.com/Katrix-/TypeNBT", Some("scm:git:github.com/Katrix-/TypeNBT"))
    ),
    homepage := Some(url("https://github.com/Katrix-/TypeNBT")),
    pomExtra := {
      <developers>
			<developer>
				<id>Katrix</id>
				<name>Nikolai Frid</name>
				<url>https://github.com/Katrix-</url>
			</developer>
		</developers>
    }
  )

lazy val typenbtJVM = typenbt.jvm
lazy val typenbtJS  = typenbt.js

lazy val example = project.dependsOn(typenbtJVM).settings(commonSettings: _*).settings(name := "examples")
