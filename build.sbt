ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "474_Project"
  )
val scalaticVersion = "3.2.9"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaticVersion % Test
libraryDependencies += "org.scalatest" %% "scalatest-featurespec" % scalaticVersion % Test
