sbtPlugin := true

name := "sbt-rats"

version := "2.2.0-SNAPSHOT"

organization := "com.googlecode.sbt-rats"

// Scala compiler settings

scalaVersion := "2.9.2"

scalacOptions ++= Seq ("-deprecation", "-unchecked")

// Interactive settings

logLevel := Level.Info

shellPrompt <<= (name, version) { (n, v) => 
     _ => n + " " + v + "> "
}

// Dependencies

libraryDependencies ++= Seq (
    "com.googlecode.kiama" % "kiama_2.9.2" % "1.3.0",
    "xtc" % "rats" % "2.3.1"
)

// Source code locations

scalaSource <<= baseDirectory { _ / "src" }

javaSource <<= baseDirectory { _ / "src" }

unmanagedSourceDirectories in Compile <<= Seq (javaSource).join

// Resources

unmanagedResourceDirectories <<= scalaSource { Seq (_) }

unmanagedResourceDirectories in Test <<= unmanagedResourceDirectories

// Test resources are the non-Scala files in the source that are not hidden
unmanagedResources in Test <<= scalaSource map { s => {
    (s ** (-"*.scala" && -HiddenFileFilter)).get
}}

// Publishing

publishTo := Some (Classpaths.sbtPluginReleases)

publishMavenStyle := false
