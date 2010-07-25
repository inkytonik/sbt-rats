/*
 * This file is part of sbt-rats.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
 */

import sbt._
import sbt.Process._

/**
 * sbt project configuration for RatsSbtPlugin
 */
class RatsSbtPluginProject (info: ProjectInfo) extends PluginProject (info)
    with posterous.Publish
{
    // Configure paths
    override def mainScalaSourcePath = "src"
    override def testScalaSourcePath = "src"

    // Set compiler options
    override def compileOptions = super.compileOptions ++ Seq (Unchecked)
    
    // Add extra files to included resources
    def extraResources = "README.txt" +++ "LICENSE"
    override def mainResources = super.mainResources +++ extraResources

    // Publish to Maven style repo at scala-tools.org
    override def managedStyle = ManagedStyle.Maven
    val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"

    // Get credentials from here
    Credentials (Path.userHome / ".ivy2" / ".credentials", log)
}
