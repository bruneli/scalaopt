organization := "org.scalaopt"

name := "scalaopt-stdapps"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.11.1", "2.11.2")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"