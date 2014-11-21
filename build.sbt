organization := "org.scalaopt"

name := "scalaopt"

lazy val root = project.in( file(".")).aggregate(algos, sparkapps)

lazy val algos = project.in( file("algos"))

lazy val sparkapps = project.in( file("spark-apps")).dependsOn(algos)

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.1", "2.10.3")
