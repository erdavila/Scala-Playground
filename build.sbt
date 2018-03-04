name := "Scala Playground"
version := "1.0"

scalaVersion in ThisBuild := "2.12.4"
scalacOptions in ThisBuild += "-feature"
scalacOptions in ThisBuild += "-deprecation"

run := (run in (root, Compile)).evaluated

lazy val macros = (project in file("macros")).settings(
  name := "Scala Playground Macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val root = (project in file(".")) dependsOn macros
