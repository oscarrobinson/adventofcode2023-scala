ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "adventofcode2023-scala"
  )
