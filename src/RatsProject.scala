/*
 * This file is part of sbt-rats.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
 */

import sbt._
import sbt.Process._

/**
 * Provides sbt tasks for use with the Rats! parser generator.  The main
 * Rats! jar file is assumed to be on the project's compile classpath.
 */
trait RatsProject extends DefaultProject
{
    /**
     * rats library
     */
    val ratslib = "rats" % "rats" % "1.14.4" from "http://cs.nyu.edu/rgrimm/xtc/rats.jar"

    /**
    * The path of the file in which the main parser module is stored.
    */
    val mainRatsModule : Path
    
    /**
     * The directory in which the main module is stored.
     */
    lazy val dir = Path.fromFile (mainRatsModule.asFile.getParent)
    
    /**
     * The basename of the main module.
     */
    lazy val base = mainRatsModule.base

    /**
     * A task to run Rats! on the main parser module.  The generated parser
     * will be written to a file with the same name as the mainRatsModule,
     * but with a .java extension.  This task depends on all .rats files
     * in the project source directories, whether they are actually used by
     * the main module or not.
    */
    lazy val rats = {
        val main = mainRatsModule
        val srcs = descendents (mainSourceRoots, "*.rats")
        val prod = dir / (base + ".java")
        val fmt = "java -cp %s xtc.parser.Rats -out %s %s"
        val classpath = Path.makeString (compileClasspath.get)
        val cmd = fmt.format (classpath, dir, main)
        fileTask ("Generate parser from " + main, prod from srcs) {
            if (cmd ! log == 0)
                None
            else
                Some ("Rats! failed")
        } describedAs ("Generate Rats! parser")
    }
    
    /**
     * Run the rats task before compilation.
     */
    override def compileAction = super.compileAction dependsOn (rats)

    /**
     * A task to delete the generated Rats! parser.
     */
    lazy val cleanRats = cleanTask (dir / (base + ".java"))

    /**
     * Make the rats clean task run on project clean.
     */
    override def cleanAction = super.cleanAction dependsOn (cleanRats)
}
