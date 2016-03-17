import scalariform.formatter.preferences._
import AssemblyKeys._
import sbtassembly.Plugin.MergeStrategy

name := "qa"

version := "1.0"

scalaVersion := "2.11.8"

sbtVersion := "0.13.8"

javaOptions += "-Xmx3G"

fork := true

resolvers += "Atilika Open Source repository" at "http://www.atilika.org/nexus/content/repositories/atilika"

assemblySettings


libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.apache.lucene" % "lucene-core" % "4.10.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.10.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.10.0",
  "org.apache.lucene" % "lucene-analyzers-kuromoji" % "4.10.0",
  "com.ibm.icu" % "icu4j" % "56.1",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "net.sf.py4j" % "py4j" % "0.9",
  "org.atilika.kuromoji" % "kuromoji" % "0.7.7",
  "org.apache.spark" % "spark-core_2.11" % "1.6.0",
  "org.apache.spark" % "spark-mllib_2.11" % "1.6.0",
  "org.apache.spark" % "spark-sql_2.11" % "1.6.0",
  "com.databricks" % "spark-csv_2.11" % "1.4.0"
)


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


dataDirectory := file("~/data")

wiki1File := (dataDirectory in Compile).value / "wiki1"

wiki2File := (dataDirectory in Compile).value / "wiki2"

wiki3File := (dataDirectory in Compile).value / "wiki3"

wiki4File := (dataDirectory in Compile).value / "jawiki" / "4"

indexFile := (resourceDirectory in Compile).value / "index"

unpackResources := {
  implicit val logger = streams.value.log
// `unpackArchive` and `downloadResource` are defined in project/Build.scala
//  downloadResource(wiki1URL.value, wiki1Pack.value)
//  downloadResource(wiki2URL.value, wiki2Pack.value)
//  downloadResource(wiki3URL.value, wiki3Pack.value)
//    downloadResource(wiki4URL.value, wiki4Pack.value)
//  downloadResource(wikiExURL.value, wikiEx.value)
//  cleanWiki(wikiEx.value, wiki1Pack.value, wiki1File.value)
//  cleanWiki(wikiEx.value, wiki2Pack.value, wiki2File.value)
//  cleanWiki(wikiEx.value, wiki3Pack.value, wiki3File.value)
//  indexWiki(wiki4File.value, indexFile.value)
}

compile in Compile := {
  unpackResources.value
  (compile in Compile).value
}

mergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".properties" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".xml" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".types" => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  case "application.conf"                            => MergeStrategy.concat
  case "unwanted.txt"                                => MergeStrategy.discard
  case x =>
    val oldStrategy = (mergeStrategy in assembly).value
    oldStrategy(x)
}


//lazy val runWrapper = taskKey[Unit]("runWrapper")

//fullRunTask(runWrapper, Runtime, "qa.main.ja.Wrapper", Def.setting{wiki4File.value}.toString,Def.setting{indexFile.value}.toString)

//TaskKey[Unit]("myTask") := (runMain in Compile).toTask("Wrapper " + Def.setting{wiki4File.value}.toString + " " + Def.setting{indexFile.value}.toString).value

//mainClass in (Compile, run) := Some("qa.main.ja.Wrapper")