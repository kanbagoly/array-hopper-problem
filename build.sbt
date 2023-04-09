ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "array-hopper-problem"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test