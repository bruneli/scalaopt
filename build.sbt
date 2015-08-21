
name := "scalaopt"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra := (
  <url>https://github.com/bruneli/scalaopt</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:bruneli/scalaopt.git</url>
      <connection>scm:git:git@github.com:bruneli/scalaopt.git</connection>
    </scm>
    <developers>
      <developer>
        <id>bruneli</id>
        <name>Renaud Bruneliere</name>
        <url>https://github.com/bruneli</url>
      </developer>
    </developers>)

lazy val commonSettings = Seq(
  organization := "com.github.bruneli.scalaopt",
  scalaVersion := "2.10.5",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val root = project.in(file(".")).aggregate(core, stdapps, sparkapps).dependsOn(core, stdapps, sparkapps)

lazy val core = project.in(file("core")).settings(commonSettings: _*)

lazy val stdapps = project.in(file("std-apps")).settings(commonSettings: _*).dependsOn(core)

lazy val sparkapps = project.in(file("spark-apps")).settings(commonSettings: _*).dependsOn(core)


