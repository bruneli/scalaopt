name := "scalaopt-sparkapps"

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

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.6.1"