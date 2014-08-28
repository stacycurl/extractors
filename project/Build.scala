import sbt._

import com.typesafe.sbt.SbtScalariform.scalariformSettings
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import org.scalastyle.sbt.ScalastylePlugin.{Settings => scalaStyleSettings}

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object extractorsBuild extends Build {
  lazy val extractors = Project(
    id = "extractors",
    base = file("."),
    settings = commonSettings,
    aggregate = Seq(extractorsCore, extractorsExamples)
  )

  lazy val extractorsCore = Project(
      id = "extractors-core",
      base = file("core"),
      settings = commonSettings // ++ Publishing.settings
    )

  lazy val extractorsExamples = Project(
    id = "extractors-examples",
    base = file("examples"),
    dependencies = Seq(extractorsCore),

    settings = commonSettings ++ Seq(
      runAllIn(Compile)
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = graphSettings ++
    // scalariformSettings ++  // uncomment to reset project formatting
    scalaStyleSettings ++ instrumentSettings ++ Seq(
      organization := "com.github.stacycurl",
      scalaVersion := "2.11.2",
      maxErrors := 1,
      parallelExecution in Test := true,
      scalacOptions       := Seq(
        "-feature",
        "-language:higherKinds",
        "-language:implicitConversions",
        "-Xfatal-warnings",
        "-deprecation",
        "-unchecked"
      ),
      libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0",
      libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0",
      initialCommands in console := """import sjc.extractors._""",
      highlighting := true,
      failOnMinimumCoverage := true,
      minimumCoverage := 83.83
    )
}
