import sbt.Def

lazy val commonSettings: Seq[Def.Setting[_]] = Seq(
  version      := "0.1.0-SNAPSHOT",
  scalaVersion := "3.1.1",
  libraryDependencies ++= Seq(
    "org.typelevel"       %% "cats-core"         % "2.7.0",
    "eu.timepit"          %% "refined"           % "0.9.28",
    "com.disneystreaming" %% "weaver-cats"       % "0.7.11" % Test,
    "com.disneystreaming" %% "weaver-scalacheck" % "0.7.11" % Test,
    "com.lihaoyi"         %% "pprint"            % "0.7.0"  % Test
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  scalacOptions ++= Seq(
//    "-language:strictEquality",
    "-no-indent",
    "-Ykind-projector"
  )
)

lazy val root = (project in file("."))
  .settings(name := "automerge-scala")
  .settings(commonSettings: _*)
  .aggregate(core, crdt)
  .dependsOn(core, crdt)

lazy val core = (project in file("modules/core"))
  .settings(commonSettings: _*)

lazy val crdt = (project in file("modules/crdt"))
  .settings(commonSettings: _*)
