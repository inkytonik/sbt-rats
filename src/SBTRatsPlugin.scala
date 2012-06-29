/*
 * This file is part of sbt-rats.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
 */

import sbt._
import Keys._
import xtc.parser.Rats

object SBTRatsPlugin extends Plugin {

    import scala.util.matching.Regex

    /**
     * The file that contains the main Rats! module. We assume that all
     * other Rats! files are brought in by this one.
     */
    val ratsMainModule = SettingKey[File] (
        "rats-main-module", "The file containing the main Rats! module"
    )

    /**
     * If true, assume that the Rats!-generated parser is to be used with
     * Scala and use Scala lists for repeated constructs. Otherwise, use
     * the default Rats! pair-based lists.
     */
    val ratsUseScalaLists = SettingKey[Boolean] (
        "rats-use-scala-lists", "Use Scala lists instead of Rats! pair-based lists"
    )

    /**
     * Run the Rats! parser generator if any of the .rats files in the source have
     * changed or the output doesn't exist.
     */
    def runRats =
        (ratsMainModule, ratsUseScalaLists,
         target, sourceManaged in Compile, streams, scalaSource, cacheDirectory) map {
            (main, useScalaLists,
             tgtDir, smDir, str, srcDir, cache) => {

                val cachedFun =
                    FileFunction.cached (cache / "sbt-rats", FilesInfo.lastModified,
                                         FilesInfo.exists) {
                        (in: Set[File]) =>
                            runRatsImpl (main, useScalaLists,
                                         tgtDir, smDir, str)
                    }

                val inputFiles = (srcDir ** "*.rats").get.toSet
                cachedFun (inputFiles).toSeq

            }
        }

    /**
     * The implementation of the invocation of Rats! Output is produced in the 
     * sbt-rats directory under the target's sourceManaged directory, which is
     * created if it doesn't exist.
     */
    def runRatsImpl (main : File, useScalaLists : Boolean,
                     tgtDir: File, smDir : File, str : TaskStreams) : Set[File] = {
        val genDir = tgtDir / "sbt-rats"
        IO.createDirectory (genDir)
        val outDir = smDir / "sbt-rats"
        IO.createDirectory (outDir)
        val mainPath = main.absolutePath
        val mainDir = main.getParentFile
        str.log.info ("Running Rats! on %s, output to %s".format (mainPath, genDir))
        val rats = new RatsRunner ()
        rats.run (Array ("-silent", "-no-exit",
                         "-in", mainDir.absolutePath,
                         "-out", genDir.absolutePath,
                         mainPath))
        if (rats.getRuntime.seenError) {
            sys.error ("Rats! failed")
            Set.empty
        } else {
            val genFiles = (genDir ** "*.java").get.toSet
            genFiles.size match {
                case 1 =>
                    val genFile = genFiles.head
                    str.log.info ("Rats! generated %s".format (genFile))
                    val outFile = outDir / genFile.name
                    if (useScalaLists) {
                        str.log.info ("Rats! transforming %s for Scala into %s".format (
                                          genFile, outFile))
                        transformForScala (genFile, outFile, useScalaLists)
                    } else {
                        str.log.info ("Rats! copying %s to %s".format (genFile, outFile))
                        IO.copyFile (genFile, outFile, true)
                    }
                    Set (outFile)
                case 0 =>
                    sys.error ("Rats! didn't generate any files")
                    Set.empty
                case _ =>
                    sys.error ("Rats! generated more than one file: %s".format (
                                    genFiles.mkString (" ")))
                    Set.empty
            }
        }
    }

    /**
     * Make the indicated replacements in the given string, returning the result.
     */
    def makeReplacements (contents : String, replacements : List[(Regex,String)]) : String =
        replacements.foldLeft (contents) {
            case (s, r) =>
                r._1.replaceAllIn (s, r._2)
        }

    /**
     * Transform the generated file into the output file as per the flag parameters.
     *  - useScalaLists: replace xtc pairs with Scala lists
     */
    def transformForScala (genFile : File, outFile : File, useScalaLists : Boolean) {

        def transformPairsToLists (contents : String) : String = {
            val pairsToLists =
                List (
                    "import xtc.util.Pair;".r ->
                        """import scala.collection.immutable.List;
                          |import scala.collection.immutable.\$colon\$colon;""".stripMargin,
                    "Pair.empty".r ->
                        "List.empty",
                    """new Pair(<[^>]+>)\(([^,)]+)\)""".r -> 
                        """new \$colon\$colon$1($2, List.empty)""",
                    """new Pair(<[^>]+>)""".r -> 
                        """new \$colon\$colon$1""",
                    "Pair(<[^>]+>)".r ->
                        "List$1"
                )
            makeReplacements (contents, pairsToLists)
        }

        val contents = IO.read (genFile)
        val contents1 = 
            if (useScalaLists)
                transformPairsToLists (contents)
            else
                contents
        IO.write (outFile, contents1)                

    }

    /**
     * Settings for the plugin:
     *  - run Rats! as a source generator
     *  - add the Rats! jar to the dependent libraries
     *  - default value for rats-use-scala-lists
     */
    val sbtRatsSettings = Seq (

        sourceGenerators in Compile <+= runRats,

        libraryDependencies ++= Seq (
            "rats" % "rats" % "2.3.1" from "http://cs.nyu.edu/rgrimm/xtc/rats.jar"
        ),

        ratsUseScalaLists := false

    )

}

/**
 * Helper class to expose the Rats! runtime which is protected.
 */
class RatsRunner extends Rats {

    def getRuntime =
        runtime

}
