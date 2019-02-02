name := "Scala Playground"
version := "1.0"

scalaVersion in ThisBuild := "2.12.6"
scalacOptions in ThisBuild += "-feature"
scalacOptions in ThisBuild += "-deprecation"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
