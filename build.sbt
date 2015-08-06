
name := "scalaopt"

lazy val commonSettings = Seq(
  organization := "com.github.bruneli.scalaopt",
  scalaVersion := "2.10.5",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val root = project.in(file(".")).aggregate(core, stdapps, sparkapps).dependsOn(core, stdapps, sparkapps)

lazy val core = project.in(file("core")).settings(commonSettings: _*)

lazy val stdapps = project.in(file("std-apps")).settings(commonSettings: _*).dependsOn(core)

lazy val sparkapps = project.in(file("spark-apps")).settings(commonSettings: _*).dependsOn(core)


