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
{
    // Configure paths
    override def mainScalaSourcePath = "src"
    override def testScalaSourcePath = "src"
    override def outputDirectoryName = "bin"

    // Set compiler options
    override def compileOptions = super.compileOptions ++ Seq (Unchecked)
    
    // Add extra files to included resources
    def extraResources = "README.txt" +++ "LICENSE"
    override def mainResources = super.mainResources +++ extraResources

}
