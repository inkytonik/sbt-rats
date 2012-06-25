/*
 * This file is part of sbt-rats.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
 */

import sbt._
import Keys._
import xtc.parser.Rats

object SBTRatsPlugin extends Plugin {

    /**
     * The file that contains the main Rats! module. We assume that all
     * other Rats! files are brought in by this one.
     */
    val ratsMainModule = SettingKey[File] (
        "rats-main-module", "The file containing the main Rats! module"
    )

    /**
     * Run the Rats! parser generator if any of the .rats files in the source have
     * changed or the output doesn't exist.
     */
    def runRats =
        (sourceManaged in Compile, streams, ratsMainModule, scalaSource, cacheDirectory) map {
            (smdir, str, main, srcdir, cache) => {

                val cachedFun =
                    FileFunction.cached (cache / "sbt-rats", FilesInfo.lastModified,
                                         FilesInfo.exists) {
                        (in: Set[File]) =>
                            runRatsImpl (smdir, str, main, srcdir)
                    }

                val inputFiles = (srcdir ** "*.rats").get.toSet
                cachedFun (inputFiles).toSeq

            }
        }

    /**
     * The implementation of the invocation of Rats! Output is produced in the 
     * sbt-rats directory under the target's sourceManaged directory, which is
     * created if it doesn't exist.
     */
    def runRatsImpl (smdir : File, str : TaskStreams, main : File, srcdir : File) : Set[File] = {
        val outdir = smdir / "sbt-rats"
        val mainPath = main.absolutePath
        IO.createDirectory (outdir)
        str.log.info ("Running Rats! on %s".format (mainPath))
        val rats = new RatsRunner ()
        rats.run (Array ("-silent", "-no-exit",
                         "-in", srcdir.absolutePath,
                         "-out", outdir.absolutePath,
                         mainPath))
        if (rats.getRuntime.seenError) {
            sys.error ("Rats failed")
            Set.empty
        } else {
            val genfiles = (outdir ** "*.java").get.toSet
            str.log.info ("Rats! generated %s".format (genfiles.mkString))
            genfiles
        }
    }

    /**
     * Settings for the plugin:
     *  - run Rats! as a source generator
     *  - make sure that the generated parser (in Java) is compiled first
     *  - add the Rats! jar to the dependent libraries
     */
    val sbtRatsSettings = Seq (

        sourceGenerators in Compile <+= runRats,

        compileOrder in Compile := CompileOrder.JavaThenScala,

        libraryDependencies ++= Seq (
            "rats" % "rats" % "2.3.1" from "http://cs.nyu.edu/rgrimm/xtc/rats.jar"
        )

    )

}

/**
 * Helper class to expose the Rats! runtime which is protected.
 */
class RatsRunner extends Rats {

    def getRuntime =
        runtime

}
