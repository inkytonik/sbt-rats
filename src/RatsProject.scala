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
     * Depend on the rats library.
     */
    override def libraryDependencies = Set (ratslib)

    /**
    * The path in which the main parser module is stored.
    */
    val mainRatsModuleDir : Path

    /**
    * The name of the main parser module (without .rats extension).
    */
    val mainRatsModule : String

    /**
    * A task to run Rats! on the main parser module.
    */
    lazy val rats = ratsTask (mainRatsModuleDir, mainRatsModule)

    /**
     * Run the rats task before compilation.
     */
    override def compileAction = super.compileAction dependsOn (rats)

    /**
     * Create a task for running the Rats! parser generator.  dir should be
     * the path in which the main Rats! module resides.  The generated parser
     * will be written there.  base should be the name of the file containing
     * the main Rats! module (minus the .rats extension).  The parser will
     * be written to dir/base.java.  This task depends on all .rats files
     * in the project source directories, whether they are actually used by
     * the main module or not.
     */
    def ratsTask (dir : Path, base : String) : Task = {
        val main = dir / (base + ".rats")
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
     * A task to delete the generated Rats! parser.
     */
    lazy val cleanRats = cleanRatsTask (mainRatsModuleDir, mainRatsModule)

    /**
     * Make the rats clean task run on project clean.
     */
    override def cleanAction = super.cleanAction dependsOn (cleanRats)

    /**
     * Delete a Rats!-generated parser.  The parameters should be the same
     * as for the corresponding ratsTask.
     */
    def cleanRatsTask (dir : Path, base : String) : Task =
        cleanTask (dir / (base + ".java"))

}
