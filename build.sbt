import ReleaseTransformations._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val intervalsetSettings = Seq(
  organization := "com.rklaehn",
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.12.1"),
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
    "org.spire-math" %% "spire" % "0.13.0",
    "com.rklaehn" %% "sonicreducer" % "0.5.0",
    "org.spire-math" %% "spire-laws" % "0.13.0" % "test",

    // thyme
    "com.github.ichoran" %% "thyme" % "0.1.2-SNAPSHOT" % "test"
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  licenses += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://github.com/rklaehn/intervalset")),

  // release stuff
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := version { v =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  }.value,
  pomExtra :=
    <scm>
      <url>git@github.com:rklaehn/intervalset.git</url>
      <connection>scm:git:git@github.com:rklaehn/intervalset.git</connection>
    </scm>
      <developers>
        <developer>
          <id>r_k</id>
          <name>R&#xFC;diger Klaehn</name>
          <url>http://github.com/rklaehn/</url>
        </developer>
      </developers>
  ,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    ReleaseStep(action = Command.process("package", _)),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges))

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val root = project.in(file("."))
  .aggregate(intervalsetJVM, intervalsetJS)
  .settings(name := "intervalset-root")
  .settings(intervalsetSettings: _*)
  .settings(noPublish: _*)

lazy val intervalset = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(name := "intervalset")
  .settings(intervalsetSettings: _*)

lazy val intervalsetJVM = intervalset.jvm
lazy val intervalsetJS = intervalset.js
