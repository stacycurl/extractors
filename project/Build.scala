import sbt._
import Keys._

import sbtbuildinfo.Plugin._

import com.typesafe.sbt.SbtGit._
import GitKeys._

import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

object extractorsBuild extends Build {

  lazy val extractors = Project(
    id = "extractors",
    base = file("."),
    aggregate = Seq(extractorsCore, extractorsExamples),
    settings = commonSettings ++ Seq(
      moduleName := "extractors-root",

      (unmanagedSourceDirectories in Compile) := Nil,
      (unmanagedSourceDirectories in Test) := Nil,

      publish := (),
      publishLocal := ()
    )
  )

  lazy val extractorsCore =
    Project(
      id = "extractors-core",
      base = file("core"),
      settings = commonSettings ++ buildInfoSettings ++ releaseSettings ++ Seq(
        moduleName := "extractors",

        managedSourceDirectories in Test := Nil,

        libraryDependencies <++= scalaVersion { sv =>
          Seq(
            "org.scala-lang" % "scala-compiler" % sv,
            "com.novocode" % "junit-interface" % "0.7" % "test",
            "org.scalaz" % "scalaz-core_2.10" % "7.1.0",
            "com.chuusai" %% "shapeless" % "2.0.0"
        )},

        initialCommands in console := """import sjc.extractors._""",

        mappings in (Compile, packageSrc) <++=
          (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
            (srcs pair (Path.relativeTo(base) | Path.flat))
          },

        mappings in (Compile, packageSrc) <++=
          (mappings in (Compile, packageSrc) in LocalProject("extractors-examples")),

        buildInfoPackage := "extractors",
        buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
        buildInfoKeys ++= Seq[BuildInfoKey](
          version,
          scalaVersion,
          gitHeadCommit,
          BuildInfoKey.action("buildTime") {
            System.currentTimeMillis
          }
        ),

        releaseProcess := Seq[ReleaseStep](
          checkSnapshotDependencies,
          inquireVersions,
          runTest,
          setReleaseVersion,
          commitReleaseVersion,
          tagRelease,
          setNextVersion,
          commitNextVersion,
          pushChanges
        )
      )
    )

  lazy val extractorsExamples = Project(
    id = "extractors-examples",
    base = file("examples"),
    dependencies = Seq(extractorsCore),

    settings = commonSettings ++ Seq(
      libraryDependencies <++= scalaVersion { sv =>
        Seq(
          "org.scala-lang" % "scala-compiler" % sv,
          "com.novocode" % "junit-interface" % "0.7" % "test"
      )},

      runAllIn(Compile),

      publish := (),
      publishLocal := ()
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = Defaults.defaultSettings ++
    Seq(
      organization        := "com.github.stacycurl",
      scalaVersion        := "2.10.3",
      scalaBinaryVersion  := "2.10.3",

      (unmanagedSourceDirectories in Compile) <<= (scalaSource in Compile)(Seq(_)),
      (unmanagedSourceDirectories in Test) <<= (scalaSource in Test)(Seq(_)),

      scalacOptions       := Seq(
        "-feature",
        "-language:higherKinds",
        "-language:implicitConversions",
        "-Xfatal-warnings",
        "-deprecation",
        "-unchecked"),

      resolvers           ++= Seq(
        Classpaths.typesafeSnapshots,
        "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
      )
    )
}
