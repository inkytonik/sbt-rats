sbtPlugin := true

name := "sbt-rats"

version in ThisBuild := "2.4.0-SNAPSHOT"

organization in ThisBuild := "org.bitbucket.inkytonik.sbt-rats"

// Scala compiler settings

scalaVersion := "2.10.6"

scalacOptions ++= Seq ("-deprecation", "-feature", "-unchecked")

// Interactive settings

logLevel := Level.Info

shellPrompt <<= (name, version) { (n, v) =>
     _ => n + " " + v + "> "
}

// Dependencies

libraryDependencies ++= Seq (
    "com.googlecode.kiama" % "kiama_2.10" % "1.8.0",
    "xtc" % "rats" % "2.4.0"
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

import bintray.Keys._

bintrayPublishSettings

licenses += ("BSD New", url (s"https://bitbucket.org/inkytonik/${name.value}/src/default/LICENSE"))

publishMavenStyle := false

repository in bintray := "sbt-plugins"

bintrayOrganization in bintray := None

vcsUrl in bintray := Some (s"https://bitbucket.org/inkytonik/${name.value}")
