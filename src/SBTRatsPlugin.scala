/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

import sbt._
import Keys._
import xtc.parser.Rats

/**
 * A structure to hold the flag values so we can pass them around together.
 */
case class Flags (
    useScalaLists : Boolean,
    useScalaPositions : Boolean,
    useScalaOptions : Boolean,
    useDefaultComments : Boolean,
    useDefaultLayout : Boolean,
    useDefaultWords : Boolean,
    defineASTClasses : Boolean,
    definePrettyPrinter : Boolean,
    useKiama : Boolean
)

object SBTRatsPlugin extends Plugin {

    import ast.Grammar
    import parser.Parser
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.IO.filereader
    import org.kiama.util.Messaging.{messagecount, resetmessages, sortedmessages}
    import scala.collection.mutable.ListBuffer
    import scala.util.matching.Regex

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
            "Use Scala lists instead of Rats! pair-based lists for repetitions"
    )

    /**
     * If true, assume that the Rats!-generated parser is to be used with
     * Scala and use Scala lists for repeated constructs. Otherwise, use
     * the default Rats! pair-based lists.
     */
    val ratsUseScalaOptions = SettingKey[Boolean] (
        "rats-use-scala-options",
            "Use Scala options instead of Rats!-style possibly-nullable fields for options"
    )

    /**
     * Set the locations of Scala Positional semantic values instead of 
     * Rats! locations. Requires the Rats! option `withLocation` to have
     * any effect.
     */
    val ratsUseScalaPositions = SettingKey[Boolean] (
        "rats-use-scala-positions",
            "Set the position of any Positional semantic values (requires Rats! withLocation option)"
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
     * If a syntax definition is being used, generate definitions for 
     * compatible abstract syntax trees as Scala case classes.
     */
    val ratsDefineASTClasses = SettingKey[Boolean] (
        "rats-define-ast-classes",
            "Define Scala classes to represent abstract syntax trees (syntax mode only)"
    )

    /**
     * If a syntax definition is being used and AST classes are being generated,
     * also generate definitions for a Kiama-based pretty printer for the AST.
     */
    val ratsDefinePrettyPrinter = SettingKey[Boolean] (
        "rats-define-pretty-printer",
            "Define Kiama-based pretty-printer for abstract syntax trees (syntax mode only, requires ratsDefineASTClasses)"
    )

    /**
     * Include support in generated components to make it easy to use them
     * with Kiama.
     */
    val ratsUseKiama = SettingKey[Boolean] (
        "rats-use-kiama",
            "Add extra support for using Kiama with generated components"
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
        val outDir = smDir / "sbt-rats"
        IO.createDirectory (outDir)
        if (main.ext == "rats")
            runRatsImpl (flags, main, genDir, outDir, str)
        else {
            runSyntaxImpl (flags, main, genDir, outDir, str) match {
                case Some ((ratsMain, otherFiles)) =>
                    runRatsImpl (flags, ratsMain, genDir, outDir, str) ++
                        otherFiles
                case _ =>
                    Set.empty
            }
        }
    }

    /**
     * Convert a syntax definition into Rats! file and other supporting Scala
     * sources. Returns None if something went wrong, otherwise returns a 
     * pair of the generated Rats! specification file and a list of other
     * files that were generated.
     */
    def runSyntaxImpl (flags : Flags, main : File, genDir : File, outDir : File,
                       str : TaskStreams) : Option[(File,List[File])] = {
        str.log.info ("Running Syntax generation on %s, output to %s and %s".format (
                          main, genDir, outDir))
        val filename = main.absolutePath
        val reader = filereader (filename)
        val p = new Parser (reader, filename)
        val pr = p.pGrammar (0)
        if (pr.hasValue) {

            // The abstract syntax tree (AST) representing the syntax
            val grammar = p.value (pr).asInstanceOf[Grammar]

            // Make an analyser for this run
            val analyser = new Analyser (flags)

            // Check AST for semantic errors
            initTree (grammar)
            resetmessages
            analyser.check (grammar)

            if (messagecount == 0) {

                // Make a desugarer for this run
                val desugarer = new Desugarer (analyser)

                // No errors, go on to desugaring, translation and generation
                val desugaredGrammar = desugarer.desugar (grammar)
                initTree (desugaredGrammar)

                // Make a translator for this run
                val translator = new Translator (analyser)

                // Generate the Rats! specification
                val name = grammar.pkg.last
                val genFile = genDir / (name + ".rats")
                str.log.info ("Syntax generating Rats! file %s".format (genFile))
                translator.translate (flags, genFile, desugaredGrammar)

                // Buffer of extra generated files
                val extraFiles = ListBuffer[File] ()

                // Make a generator for this run
                val generator = new Generator (analyser)

                // If requested, generate the AST classes
                if (flags.defineASTClasses) {
                    val astFile = outDir / "Syntax.scala"
                    str.log.info ("Syntax generating AST classes %s".format (astFile))
                    generator.generateASTClasses (flags, astFile, grammar)
                    extraFiles.append (astFile)
                }

                // If requested, generate the AST classes
                if (flags.defineASTClasses && flags.definePrettyPrinter) {
                    val ppFile = outDir / "PrettyPrinter.scala"
                    str.log.info ("Syntax AST pretty-printer %s".format (ppFile))
                    generator.generatePrettyPrinter (flags, ppFile, grammar)
                    extraFiles.append (ppFile)
                }

                // If requested, generate support files
                if (flags.useScalaLists) {
                    val supportFile = outDir / "ParserSupport.scala"
                    str.log.info ("Rats! generating Scala support file %s".format (
                                      supportFile))
                    generator.generateSupportFile (flags, supportFile)
                    extraFiles.append (supportFile)
                }

                Some ((genFile, extraFiles.result ()))

            } else {

                // str.log.error ("Syntax semantic analysis of %s failed".format (main))
                for (record <- sortedmessages)
                    str.log.error (record.toString)
                sys.error ("Syntax semantic analysis of %s failed".format (filename))
                None

            }

        } else {

            str.log.info ("Syntax parsing %s failed".format (main))
            sys.error (p.format (pr.parseError))
            None

        }
    }

    /**
     * Run Rats! on the `main` file.
     */
    def runRatsImpl (flags : Flags, main : File, genDir : File, outDir : File,
                     str : TaskStreams) : Set[File] = {
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
                        Set (outFile)

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
     *  - useScalaPositions: replace Rats! location code, gen LineColPosition class
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

        def transformNullablesToOptions (contents : String) : String = {
            val nullablesToOptions =
                List (
                    """import xtc\.util\.Pair;""".r ->
                        """import xtc.util.Pair;
                          |import scala.Option;""".stripMargin,
                    """(\s+)(\w+)\s+(yyOpValue[0-9]+);""".r ->
                        """$1Option<$2> $3;""",
                    """(yyOpValue[0-9]+) = (v[^;]+);""".r ->
                        """$1 = Option.apply ($2);""",
                    """(yyOpValue[0-9]+) = null;""".r ->
                        """$1 = Option.empty();""",
                    """(\w+) (v[0-9]+) = (yyOpValue[0-9]+);""".r ->
                        """Option<$1> $2 = $3;"""
                )
            makeReplacements (contents, nullablesToOptions)
        }

        def transformPositions (contents : String) : String = {

            val locatablesToPositionsScala =
                List (
                    """import xtc\.tree\.Locatable;""".r ->
                        """import scala.util.parsing.input.Positional;
                          |import scala.util.parsing.input.Position;
                          |import sbtrats.LineColPosition;""".stripMargin,
                    """Locatable""".r ->
                        """Positional""",
                    """public final class (\w+) extends ParserBase \{""".r ->
                        """
                        |public final class $1 extends ParserBase {
                        |
                        |  /** Set position of a Positional */
                        |  void setLocation(final Positional positional, final int index) {
                        |    if (null != positional) {
                        |      Column c = column(index);
                        |      positional.setPos(new LineColPosition(c.line, c.column));
                        |    }
                        |  }
                        |""".stripMargin
                )

            val locatablesToPositionsKiama =
                List (
                    """import xtc\.tree\.Locatable;""".r ->
                        """import org.kiama.util.Positioned;
                          |import scala.util.parsing.input.Position;
                          |import sbtrats.LineColPosition;""".stripMargin,
                    """Locatable""".r ->
                        """Positioned""",
                    """public final class (\w+) extends ParserBase \{""".r ->
                        """
                        |public final class $1 extends ParserBase {
                        |
                        |  /** Set position of a Positioned */
                        |  void setLocation(final Positioned positional, final int start) {
                        |    if (null != positional) {
                        |      Column s = column(start);
                        |      positional.setStart(new LineColPosition(s.line, s.column));
                        |      Column f = column(yyCount == 0 ? 0 : yyCount - 1);
                        |      positional.setFinish(new LineColPosition(f.line, f.column));
                        |    }
                        |  }
                        |""".stripMargin
                )

            makeReplacements (contents, if (flags.useKiama)
                                            locatablesToPositionsKiama
                                        else
                                            locatablesToPositionsScala)

        }

        val contents = IO.read (genFile)

        val contents1 = 
            if (flags.useScalaLists)
                transformPairsToLists (contents)
            else
                contents

        val contents2 = 
            if (flags.useScalaOptions)
                transformNullablesToOptions (contents1)
            else
                contents1

        val contents3 =
            if (flags.useScalaPositions) {
                transformPositions (contents2)
            } else
                contents2

        IO.write (outFile, contents3)

    }

    /**
     * Settings for the plugin:
     *  - run Rats! as a source generator
     *  - add the Rats! jar to the dependent libraries
     *  - default values for settings
     *  - group settings together to pass around
     */
    val sbtRatsSettings = Seq (

        sourceGenerators in Compile <+= runGenerators,

        libraryDependencies ++= Seq (
            "xtc" % "rats" % "2.3.1"
        ),

        ratsUseScalaLists := false,

        ratsUseScalaPositions := false,

        ratsUseScalaOptions := false,

        ratsUseDefaultLayout := true,

        ratsUseDefaultWords := true,

        ratsUseDefaultComments := true,

        ratsDefineASTClasses := false,

        ratsDefinePrettyPrinter := false,

        ratsUseKiama := false,

        ratsFlags <<= (ratsUseScalaLists, ratsUseScalaPositions,
                       ratsUseScalaPositions, ratsUseDefaultComments,
                       ratsUseDefaultLayout, ratsUseDefaultWords,
                       ratsDefineASTClasses, ratsDefinePrettyPrinter,
                       ratsUseKiama) {
            (lists, options, posns, comments, layout, words, ast, pp,
             kiama) =>
                Flags (lists, options, posns, comments, layout, words, ast, pp,
                       kiama)
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
