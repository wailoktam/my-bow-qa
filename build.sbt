name := "qa"

version := "1.0"

scalaVersion := "2.11.6"

sbtVersion := "0.13.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.apache.lucene" % "lucene-core" % "4.10.0",
  "org.apache.lucene" % "lucene-queryparser" % "4.10.0",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.10.0",
  "org.apache.lucene" % "lucene-analyzers-kuromoji" % "4.10.0",
  "com.ibm.icu" % "icu4j" % "55.1"
).map(_ withSources() withJavadoc())

