import scalariform.formatter.preferences._

name := "qa"

version := "1.0"

scalaVersion := "2.11.6"

sbtVersion := "0.13.8"

javaOptions += "-Xmx3G"

fork := true

resolvers += DefaultMavenRepository

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.apache.lucene" % "lucene-core" % "4.10.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.10.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.10.0",
  "org.apache.lucene" % "lucene-analyzers-kuromoji" % "4.10.0",
  "com.ibm.icu" % "icu4j" % "55.1",
  "org.apache.commons" % "commons-lang3" % "3.4"
).map(_ withSources() withJavadoc())


// This settings is from plugin "sbt-start-script", which makes task "start-script" available
// "start-script" creates a script "target/start" for running the application without sbt
// usage: start <class-name> <parameters>
// e.g. ./target/start tifmo.demo.FraCas input/fracas.xml

seq(com.typesafe.sbt.SbtStartScript.startScriptForClassesSettings: _*)


// This setting enables sbt-scalariform, an sbt plugin adding support for source code formatting using Scalariform
// It adds the task `scalariformFormat` in the scopes `compile` and `test`, and run this task automatically when compiling

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(IndentPackageBlocks, false)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(IndentSpaces, 2)
  .setPreference(IndentWithTabs, false)

