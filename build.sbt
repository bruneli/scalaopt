organization := "org.scalaopt"

name := "scalaopt"

lazy val root = project.in( file(".")).aggregate(algos, stdapps, sparkapps).dependsOn(algos, stdapps, sparkapps)

lazy val algos = project.in( file("algos"))

lazy val stdapps = project.in( file("std-apps")).dependsOn(algos)

lazy val sparkapps = project.in( file("spark-apps")).dependsOn(algos)

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.11.1", "2.11.2")
