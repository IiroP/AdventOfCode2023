val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2023",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0"
  )
