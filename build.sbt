name := "Astraea"

version := "1.0"

scalaVersion := "2.12.8"

EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.12" % "1.3.6",
  "com.github.spinalhdl" % "spinalhdl-lib_2.12" % "1.3.6"
)

libraryDependencies += "com.typesafe" % "config" % "1.3.4"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

fork := true
