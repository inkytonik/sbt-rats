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

    def runRats =
        (sourceManaged in Compile, streams, ratsMainModule, scalaSource) map {
            (smdir, str, main, srcdir) => {
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
                    Nil
                } else {
                    val genfiles = (outdir ** "*.java").get
                    str.log.info ("Rats! generated %s".format (genfiles.mkString))
                    genfiles
                }
            }
        }

    // FIXME: avoid duplication of directory used for generated sources
    // FIXME: avoid duplication of rats lib here and in build.sbt of plugin

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
