import scoverage.ScoverageSbtPlugin.ScoverageKeys._

import CoverallsPlugin.CoverallsKeys._

name := "intervalset"

scalaVersion := "2.11.4"

version := "0.1-SNAPSHOT"

libraryDependencies += "org.spire-math" %% "spire" % "0.10.1"

libraryDependencies += "org.spire-math" %% "spire-scalacheck-binding" % "0.10.1" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"

libraryDependencies += "com.rklaehn" %% "abc" % "0.1-SNAPSHOT" % "test"

unmanagedBase in Test <<= baseDirectory { base => base / "test-lib" }

coverageMinimum := 100

coverageFailOnMinimum := true

coverallsTokenFile := "coveralls.token"

scalacOptions ++= Seq("-unchecked", "-feature")

initialCommands in console += """
import com.rklaehn.interval._
import spire.math._
import spire.implicits._
"""