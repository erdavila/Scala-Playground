name := "Scala Playground"
version := "1.0"

scalaVersion in ThisBuild := "2.12.6"
scalacOptions in ThisBuild += "-feature"
scalacOptions in ThisBuild += "-deprecation"

run := (run in (main, Compile)).evaluated

lazy val macros = (project in file("macros"))
  .settings(
    name := "Scala Playground Macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val main = (project in file("main"))
  .settings(
    name := "Scala Playground Main",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
  )
  .dependsOn(macros)
