sbtPlugin := true

name := "sbt-rats"

version := "2.7.0"

organization := "org.bitbucket.inkytonik.sbt-rats"

// Scala compiler settings

scalaVersion := "2.12.10"

scalacOptions := {
    val lintOption =
        if (scalaVersion.value.startsWith ("2.10"))
            "-Xlint"
        else
            "-Xlint:-stars-align,_"
    Seq (
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xfatal-warnings",
        "-Xcheckinit",
        lintOption
    )
}

scalacOptions in (Compile, doc) := 
    Seq(
        "-deprecation",
        "-feature",
        "-unchecked"
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

description := "sbt-rats provides a plugin that enables the Rats! parser generator to be used in Scala projects."
licenses += ("BSD New", url (s"https://bitbucket.org/inkytonik/${name.value}/src/master/LICENSE"))
publishMavenStyle := false
bintrayRepository := "sbt-plugins"
bintrayOrganization in bintray := None
bintrayVcsUrl := Some (s"https://bitbucket.org/inkytonik/${name.value}")
