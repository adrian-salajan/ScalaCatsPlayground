name := "ScalaPlayground"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"


libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-macros" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-kernel" % "1.6.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test