/*
 * This file is part of sbt-rats.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
 */

import sbt._
import Keys._
import org.kiama.output.PrettyPrinter
import xtc.parser.Rats

/**
 * A structure to hold the flag values so we can pass them around together.
 */
case class Flags (
    useScalaLists : Boolean,
    useDefaultComments : Boolean,
    useDefaultLayout : Boolean,
    useDefaultWords : Boolean
)

// FIXME: remove prettyprinter

object SBTRatsPlugin extends Plugin with PrettyPrinter {

    import ast.Grammar
    import Desugarer.desugar
    import parser.Parser
    import org.kiama.util.IO.filereader
    import scala.util.matching.Regex
    import Translator.translate

    /**
     * The file that contains the main Rats! module or main syntax
     * definition.
     */
    val ratsMainModule = SettingKey[File] (
        "rats-main-module",
            "The main Rats! module or syntax definition"
    )

    /**
     * If true, assume that the Rats!-generated parser is to be used with
     * Scala and use Scala lists for repeated constructs. Otherwise, use
     * the default Rats! pair-based lists.
     */
    val ratsUseScalaLists = SettingKey[Boolean] (
        "rats-use-scala-lists",
            "Use Scala lists instead of Rats! pair-based lists"
    )

    /**
     * If a syntax definition is being used, generate a default specification
     * for comments.
     */
    val ratsUseDefaultComments = SettingKey[Boolean] (
        "rats-use-default-comments",
            "Use a default definition for comments (syntax mode only)"
    )

    /**
     * If a syntax definition is being used, generate a default specification
     * for layout (i.e., whitespace and comment handling).
     */
    val ratsUseDefaultLayout = SettingKey[Boolean] (
        "rats-use-default-layout",
            "Use a default definition for layout (syntax mode only)"
    )

    /**
     * If a syntax definition is being used, generate a default specification
     * for words (i.e., letter sequences, commonly used to match identifiers
     * and the like).
     */
    val ratsUseDefaultWords = SettingKey[Boolean] (
        "rats-use-default-words",
            "Use a default definition for words (syntax mode only)"
    )

    /**
     * Aggregation of all flag settings.
     */
    val ratsFlags = SettingKey[Flags] (
        "rats-flags", "All sbt-rats flags"
    )

    /**
     * Run the generators if any of the .rats or .syntax files in the source
     * have changed or the output doesn't exist.
     */
    def runGenerators =
        (ratsFlags, ratsMainModule, scalaSource, target, sourceManaged in Compile,
         streams, cacheDirectory) map {
            (flags, main, srcDir, tgtDir, smDir, str, cache) => {

                val cachedFun =
                    FileFunction.cached (cache / "sbt-rats", FilesInfo.lastModified,
                                         FilesInfo.exists) {
                        (in: Set[File]) =>
                            runGeneratorsImpl (flags, main, tgtDir, smDir, str)
                    }

                val inputFiles = (srcDir ** ("*.rats" | "*.syntax")).get.toSet
                cachedFun (inputFiles).toSeq

            }
        }

    /**
     * Run the generator(s). If `main` is a .rats file, then just use Rats! on it.
     * Otherwise, assume it's a syntax definition, translate it and use Rats! on
     * the result.
     */
    def runGeneratorsImpl (flags : Flags, main : File,
                           tgtDir: File, smDir : File,
                           str : TaskStreams) : Set[File] = {
        val genDir = tgtDir / "sbt-rats"
        IO.createDirectory (genDir)
        if (main.ext == "rats")
            runRatsImpl (flags, main, tgtDir, genDir, smDir, str)
        else {
            runSyntaxImpl (flags, main, genDir, str) match {
                case Some (ratsMain) =>
                    runRatsImpl (flags, ratsMain, tgtDir, genDir, smDir, str)
                case _ =>
                    Set.empty
            }
        }
    }

    /**
     * Convert a syntax definition into Rats! file and other supporting Scala
     * sources.
     */
    def runSyntaxImpl (flags : Flags, main : File, genDir : File,
                       str : TaskStreams) : Option[File] = {
        str.log.info ("Running Syntax generation on %s, output to %s".format (
                          main, genDir))
        val filename = main.absolutePath
        val reader = filereader (filename)
        val p = new Parser (reader, filename)
        val pr = p.pGrammar (0)
        if (pr.hasValue) {
            val grammar = p.value (pr).asInstanceOf[Grammar]
            val desugaredGrammar = desugar (grammar)
            str.log.info (pretty_any (desugaredGrammar))
            val ratsCode = translate (flags, desugaredGrammar)
            val name = grammar.pkg.last
            val genFile = genDir / (name + ".rats")
            IO.write (genFile, ratsCode)
            Some (genFile)
        } else {
            str.log.info ("Parsing Syntax %s failed".format (main))
            sys.error (p.format (pr.parseError))
            None
        }
    }

    /**
     * Run Rats! on the `main` file.
     */
    def runRatsImpl (flags : Flags, main : File,
                     tgtDir: File, genDir : File, smDir : File,
                     str : TaskStreams) : Set[File] = {
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
                    if (flags.useScalaLists) {
                        str.log.info ("Rats! transforming %s for Scala into %s".format (
                                          genFile, outFile))
                        transformForScala (flags, genFile, outFile)
                        val supportFile = outDir / "ParserSupport.scala"
                        writeSupportFile (supportFile)
                        Set (outFile, supportFile)
                    } else {
                        str.log.info ("Rats! copying %s to %s".format (genFile, outFile))
                        IO.copyFile (genFile, outFile, true)
                        Set (outFile)
                    }
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
    def transformForScala (flags : Flags, genFile : File, outFile : File) {

        def transformPairsToLists (contents : String) : String = {
            val pairsToLists =
                List (
                    """import xtc\.util\.Pair;""".r ->
                        """import xtc.util.Pair;
                          |import scala.collection.immutable.List;
                          |import scala.collection.immutable.Nil\$;
                          |import scala.collection.immutable.\$colon\$colon;
                          |import sbtrats.ParserSupport;""".stripMargin,
                    """Pair\.empty\(\)""".r ->
                        """List.empty()""",
                    """new Pair<[^>]+>\(([^,)]+)\)""".r ->
                        """Nil$.MODULE$.\$colon\$colon($1)""",
                    """new Pair(<[^>]+>)""".r ->
                        """new \$colon\$colon$1""",
                    """Pair<Pair(<[^>]+>)>""".r ->
                        """List<List$1>""",
                    """Pair(<[^>]+>)""".r ->
                        """List$1"""
                )
            makeReplacements (contents, pairsToLists)
        }

        val contents = IO.read (genFile)
        val contents1 = 
            if (flags.useScalaLists)
                transformPairsToLists (contents)
            else
                contents
        IO.write (outFile, contents1)                

    }

    /**
     * Write module of supporting code for Scala-based parsers.
     */
    def writeSupportFile (supportFile : File) {

        val contents = """
            |// AUTOMATICALLY GENERATED by sbt-rats - EDIT AT YOUR OWN RISK
            |
            |package sbtrats
            |
            |trait Action[T] {
            |    def run (arg : T) : T
            |}
            |
            |object ParserSupport {
            |
            |    def apply[T] (actions : List[Action[T]], seed : T) : T = {
            |        var result = seed
            |        for (action <- actions) {
            |            result = action.run (result)
            |        }
            |        result
            |    }
            |
            |}
            |""".stripMargin

        IO.write (supportFile, contents)        

    }

    /**
     * Settings for the plugin:
     *  - run Rats! as a source generator
     *  - add the Rats! jar to the dependent libraries
     *  - default value for rats-use-scala-lists
     */
    val sbtRatsSettings = Seq (

        sourceGenerators in Compile <+= runGenerators,

        libraryDependencies ++= Seq (
            "rats" % "rats" % "2.3.1" from "http://cs.nyu.edu/rgrimm/xtc/rats.jar"
        ),

        ratsUseScalaLists := false,

        ratsUseDefaultLayout := true,

        ratsUseDefaultWords := true,

        ratsUseDefaultComments := true,

        ratsFlags <<= (ratsUseScalaLists, ratsUseDefaultComments, ratsUseDefaultLayout,
                       ratsUseDefaultWords) { (lists, comments, layout, words) =>
            Flags (lists, comments, layout, words)
        }

    )

}

/**
 * Helper class to expose the Rats! runtime which is protected.
 */
class RatsRunner extends Rats {

    def getRuntime =
        runtime

}
