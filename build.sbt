sbtPlugin := true

name := "sbt-rats"

version in ThisBuild := "2.7.0-SNAPSHOT"

organization in ThisBuild := "org.bitbucket.inkytonik.sbt-rats"

// Scala compiler settings

scalaVersion := "2.12.10"

scalacOptions ++=
    Seq (
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xfatal-warnings",
        "-Xcheckinit",
        "-Xlint:-stars-align,_"
    )

scalaCompilerBridgeSource := {
  val sv = appConfiguration.value.provider.id.version
  ("org.scala-sbt" % "compiler-interface" % sv % "component").sources
}

// sbt settings

sbtVersion in Global := "1.3.9"

crossSbtVersions := Vector ("1.3.9", "0.13.18")

// Interactive settings

logLevel := Level.Info

shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + (sbtVersion in pluginCrossBuild).value + " " +
            scalaVersion.value + "> "
}

// Dependencies

libraryDependencies ++= Seq (
    "com.googlecode.kiama" %% "kiama" % "1.8.0",
    "xtc" % "rats" % "2.4.0"
)

// Publishing

import bintray.Keys._

bintrayPublishSettings

licenses += ("BSD New", url (s"https://bitbucket.org/inkytonik/${name.value}/src/master/LICENSE"))

publishMavenStyle := false

repository in bintray := "sbt-plugins"

bintrayOrganization in bintray := None

vcsUrl in bintray := Some (s"https://bitbucket.org/inkytonik/${name.value}")
