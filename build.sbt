import scoverage.ScoverageSbtPlugin.ScoverageKeys._

name := "intervalset"

scalaVersion := "2.11.4"

version := "0.1-SNAPSHOT"

libraryDependencies += "org.spire-math" %% "spire" % "0.8.2" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"

unmanagedBase in Test <<= baseDirectory { base => base / "test-lib" }

coverageMinimum := 70

coverageFailOnMinimum := true
