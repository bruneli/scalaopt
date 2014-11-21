organization := "org.scalaopt"

name := "scalaopt-sparkapps"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.11.1", "2.10.3")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.apache.spark" % "spark-core_2.10" % "1.1.0"