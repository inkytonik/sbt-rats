sbtPlugin := true

name := "sbt-rats"

version := "2.0.0-SNAPSHOT"

organization := "com.googlecode.sbt-rats"

// Scala compiler settings

scalaVersion := "2.9.1"

scalacOptions ++= Seq ("-deprecation", "-unchecked")

// Interactive settings

logLevel := Level.Info

shellPrompt <<= (name, version) { (n, v) => 
     _ => n + " " + v + "> "
}

// Dependencies

libraryDependencies ++= Seq (
    "rats" % "rats" % "2.3.1" from "http://cs.nyu.edu/rgrimm/xtc/rats.jar"
)

// Source code locations

// Specify how to find source and test files.  Main sources are
//    - in src directory
//    - all .scala files, except
// Test sources, which are
//    - files whose names end in Tests.scala, which are actual test sources
//    - Scala files within the examples src

scalaSource <<= baseDirectory { _ / "src" }

unmanagedSources in Test <<= scalaSource map { s => {
    val egs = s / "org" / "kiama" / "example" ** "*.scala"
    ((s ** "*Tests.scala") +++ egs).get
}}

unmanagedSources in Compile <<= (scalaSource, unmanagedSources in Test) map { (s, tests) =>
    ((s ** "*.scala") --- tests).get
}

// Resources

unmanagedResourceDirectories <<= scalaSource { Seq (_) }

unmanagedResourceDirectories in Test <<= unmanagedResourceDirectories

// Test resources are the non-Scala files in the source that are not hidden
unmanagedResources in Test <<= scalaSource map { s => {
    (s ** (-"*.scala" && -HiddenFileFilter)).get
}}
