def plugin(moduleID: ModuleID) = // Workaround http://github.com/sbt/sbt/issues/1439
  Defaults.sbtPluginExtra(moduleID, "0.13", "2.10") excludeAll ExclusionRule("org.scala-lang")

libraryDependencies ++= Seq(
  plugin("com.timushev.sbt"  %  "sbt-updates"           % "0.1.6"),
  plugin("net.virtual-void"  %  "sbt-dependency-graph"  % "0.7.4"),
  plugin("com.typesafe.sbt"  %  "sbt-scalariform"       % "1.3.0"),
  plugin("org.scalastyle"    %% "scalastyle-sbt-plugin" % "0.5.0"),
  plugin("org.scoverage"     %% "sbt-scoverage"         % "0.99.7.1")
/*
  plugin("com.typesafe.sbt"  %  "sbt-pgp"               % "0.8.1"),
  plugin("com.github.gseitz" %  "sbt-release"           % "0.7.1"),
  plugin("com.eed3si9n"      %  "sbt-buildinfo"         % "0.2.5"),
  plugin("com.typesafe.sbt"  %  "sbt-git"               % "0.6.2")
*/
)

scalacOptions += "-deprecation"
