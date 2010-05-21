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
    import scala.util.matching.Regex
    
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
     * Path of the generated parser module.
     */
    lazy val parserPath = dir / (base + ".java")
    
    /**
     * By default, the generated Rats! parser is used as-is.  If this flag
     * is turned on, the parser is post-processed to make it more compatible
     * with use from Scala.
     */
    val ratsUseScala = false

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
        val fmt = "java -cp %s xtc.parser.Rats -out %s %s"
        val classpath = Path.makeString (compileClasspath.get)
        val cmd = fmt.format (classpath, dir, main)
        val ratsTask = 
            fileTask ("Generate parser from " + main, parserPath from srcs) {
                if (cmd ! log == 0)
                    None
                else
                    Some ("Rats! failed")
            } describedAs ("Generate Rats! parser")
        if (ratsUseScala)
            ratsTask && postProcessTask
        else
            ratsTask
    }
    
    /**
     * Run the rats task before compilation.
     */
    override def compileAction = super.compileAction dependsOn (rats)
    
    /**
     * Return a task to post-process the generated parser.  The new version
     * is stored in the same place as the original.
     * FIXME: maybe should use two different paths
     */
    private def postProcessTask =
        task {
            FileUtilities.readString (parserPath.asFile, log) match {
                case Left (s) =>
                    error (s)
                case Right (s) =>
                    val t = postProcess (s)
                    FileUtilities.write (parserPath.asFile, t, log)
            }
            None
        }

    /**
     * Actually perform the transformations to post process the parser.
     */
    def postProcess (contents : String) : String = {
        val map =
            Map (
                 // Replace xtc pair lists with Scala lists
                 "import xtc.util.Pair;" ->
                     """import scala.collection.immutable.List;
                       |import scala.collection.immutable.$colon$colon;""".stripMargin,
                 "Pair.empty()" -> "List.empty()",
                 "new Pair<([^>]+)>".r -> """new \$colon\$colon<$1>""",
                 "Pair<([^>]+)>".r -> "List<$1>"
            )

        var s = contents
        for ((from,to) <- map) {
            from match {
                case t : String => s = s.replace (t, to)
                case r : Regex  => s = r.replaceAllIn (s, to)
            }
        }
        s
    }

    /**
     * A task to delete the generated Rats! parser.
     */
    lazy val cleanRats = cleanTask (dir / (base + ".java"))

    /**
     * Make the rats clean task run on project clean.
     */
    override def cleanAction = super.cleanAction dependsOn (cleanRats)
}
