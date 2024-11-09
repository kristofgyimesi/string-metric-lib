ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

crossScalaVersions := Seq("2.12.19")

lazy val root = (project in file("."))
  .settings(
    organization         := "io.github.kristofgyimesi",
    organizationName     := "Kristof Gyimesi",
    organizationHomepage := Some(url("https://github.com/kristofgyimesi")),
    name                 := "string-metric-lib",
    description          := "A highly customizable string metrics library.",
  )

libraryDependencies ++= Seq(
  // Testing
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)
