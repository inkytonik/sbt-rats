/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012-2020 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

import sbt._
import Keys._
import xtc.parser.Rats
import xtc.tree.Node
import xtc.util.Runtime

/**
 * The possible sequence data structures that can be used with Scala
 * to represent repetitions.
 */
sealed abstract class ScalaSequenceType

/**
 * Use Scala lists to represent repetitions.
 */
case object ListType extends ScalaSequenceType

/**
 * Use Scala vectors to represent repetitions.
 */
case object VectorType extends ScalaSequenceType

/**
 * A structure to hold the flag values so we can pass them around together.
 */
case class Flags (
    scalaRepetitionType : Option[ScalaSequenceType],
    useScalaPositions : Boolean,
    useScalaOptions : Boolean,
    useDefaultComments : Boolean,
    useDefaultSpacing : Boolean,
    useDefaultLayout : Boolean,
    useDefaultWords : Boolean,
    defineASTClasses : Boolean,
    definePrettyPrinter : Boolean,
    precomputeHashCodes : Boolean,
    includeKeywordTable : Boolean,
    includeBinarySupport : Boolean,
    useKiama : Int,
    kiamaPkg : String
)

object SBTRatsPlugin extends AutoPlugin {

    import ast.Grammar
    import parser.{LineColPosition, Parser}
    import org.kiama.attribution.Attribution.{initTree, resetMemo}
    import org.kiama.util.IO.filereader
    import org.kiama.util.Message
    import org.kiama.util.Messaging.sortmessages
    import scala.collection.mutable.ListBuffer
    import scala.language.existentials
    import scala.util.matching.Regex
    import xtc.parser.ParseError

    override def requires = plugins.JvmPlugin
    override def trigger = allRequirements

    object autoImport {

        /**
        * The file that contains the main Rats! module or main syntax
        * definition.
        */
        val ratsMainModule = SettingKey[Option[File]] (
            "rats-main-module",
                "The main Rats! module. If not set, use all syntax definitions."
        )

        /**
        * If set, assume that the Rats!-generated parser is to be used with
        * Scala and use the specified type for repeated constructs. Otherwise,
        * use the default Rats! pair-based lists.
        */
        val ratsScalaRepetitionType = SettingKey[Option[ScalaSequenceType]] (
            "rats-scala-repetition-type",
                "Form of Scala data to use for repetitions. If not set, use Rats! pair-based lists."
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
        * for spacing (i.e., whitespace and comment handling).
        */
        val ratsUseDefaultSpacing = SettingKey[Boolean] (
            "rats-use-default-spacing",
                "Use a default definition for spacing (syntax mode only)"
        )

        /**
        * If a syntax definition is being used, generate a default specification
        * for layout (i.e., Space and EOL).
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
         * If a syntax definition is being used and AST classes are being generated,
         * AST nodes will precompute and cache [[java.lang.Object#hashCode()]].
         * This can improve performance when using attributed grammars.
         */
        val ratsPrecomputeHashCodes = SettingKey[Boolean] (
            "rats-precompute-hash-codes",
            "Precompute hashes for AST nodes (syntax mode only, requires ratsDefineASTClasses)"
        )

        /**
        * If non-zero, include support in generated components to make it easy to use
        * them with Kiama. Value is the major Kiama version that is being used.
        */
        val ratsUseKiama = SettingKey[Int] (
            "rats-use-kiama",
                "Major version of Kiama that should be used (default 0 means don't use Kiama)"
        )

        /**
        * Include support for keyword handling by building a table of all of the
        * keywords from the specification.
        */
        val ratsIncludeKeywordTable = SettingKey[Boolean] (
            "rats-include-keyword-table",
                "Add a table containing all keywords in the specification (syntax mode only)"
        )

        /**
        * Include support for parsing binary formats.
        */
        val ratsIncludeBinarySupport = SettingKey[Boolean] (
            "rats-include-binary-support",
                "Add extra support for using parsing binary data (syntax mode only)"
        )

        /**
        * Aggregation of all flag settings.
        */
        val ratsFlags = SettingKey[Flags] (
            "rats-flags", "All sbt-rats flags"
        )

    }

    import autoImport._

    /**
     * Run the generators if any of the .rats or .syntax files in the source
     * have changed or the output doesn't exist.
     */
    val runGenerators =
        Def.task {
            val flags = ratsFlags.value
            val main = ratsMainModule.value
            val srcDir = (scalaSource in Compile).value
            val tgtDir = target.value
            val smDir = (sourceManaged in Compile).value
            val str = streams.value

            val cache = str.cacheDirectory

            val cachedFun =
                FileFunction.cached (cache / "sbt-rats", FilesInfo.lastModified,
                                     FilesInfo.exists) {
                    (inFiles: Set[File]) =>
                        runGeneratorsImpl (flags, main, inFiles, srcDir, tgtDir,
                                        smDir, str)
                }

            val inputFiles = (srcDir ** ("*.rats" | "*.syntax")).get.toSet
            cachedFun (inputFiles).toSeq
        }

    /**
     * Run the generator(s). Use Rats! on either the main Rats! file, or on
     * the translation of all syntax definition files, whichever applies.
     */
    def runGeneratorsImpl (flags : Flags, optmain : Option[File], inFiles : Set[File],
                           srcDir : File, tgtDir: File, smDir : File,
                           str : TaskStreams) : Set[File] = {

        // Set up output directories
        val genDir = tgtDir / "sbt-rats"
        IO.createDirectory (genDir)
        val outDir = smDir / "sbt-rats"
        IO.createDirectory (outDir)

        // Make an analyser for this run
        val analyser = new Analyser (flags)

        // Make a generator for this run
        val generator = new Generator (analyser)

        // Buffer of generated files
        val generatedFiles = ListBuffer[File] ()

        // If some Scala support is requested, generate Scala support file
        if (flags.scalaRepetitionType != None | flags.useScalaOptions | flags.useScalaPositions) {
            val supportFile = outDir / "sbtrats" / "ParserSupport.scala"
            str.log.info (s"Rats! generating Scala support file $supportFile")
            generator.generateSupportFile (flags, supportFile)
            generatedFiles.append (supportFile)
        }

        def processSyntaxFile (syntaxFile : File) {
            runSyntaxImpl (flags, syntaxFile, genDir, outDir, str,
                           analyser, generator) match {
                case Some ((_, mainFile, newFiles)) =>
                    generatedFiles.appendAll (newFiles)
                    val javaFiles = runRatsImpl (flags, mainFile, false, srcDir,
                                                 genDir, outDir, str)
                    generatedFiles.appendAll (javaFiles)
                case None =>
                    // Do nothing
            }
        }

        // Check for a main Rats! module
        optmain match {

            case Some (main) =>
                str.log.info (s"Rats! got a .rats file $main")
                // Got one, just run Rats! on it and we're done.
                val newFiles = runRatsImpl (flags, main, true, srcDir, genDir,
                                            outDir, str)
                generatedFiles.appendAll (newFiles)

            case None =>
                // Otherwise, we translate all syntax definitions into their
                // own parser, with optional auxiliary files.

                // Translate each of the syntax files, then run Rats! on result
                // collect generated files
                for (inFile <- inFiles)
                    if (inFile.ext == "syntax")
                        processSyntaxFile (inFile)

        }

        // Return all of the generated files
        generatedFiles.result ().toSet

    }

    /**
     * Convert a syntax definition into Rats! file and other supporting Scala
     * sources. Returns None if something went wrong, otherwise returns a
     * pair of the generated Rats! specification file and a list of other
     * files that were generated.
     */
    def runSyntaxImpl (flags : Flags, main : File, genDir : File, outDir : File,
                       str : TaskStreams, analyser : Analyser,
                       generator : Generator) :
                          Option[(File,File,List[File])] = {
        str.log.info (s"Running Syntax generation on $main, output to $genDir and $outDir")
        val filename = main.absolutePath
        val reader = filereader (filename)
        val p = new Parser (reader, filename)
        val pr = p.pGrammar (0)
        if (pr.hasValue) {

            // The abstract syntax tree (AST) representing the syntax
            val grammar = p.value (pr).asInstanceOf[Grammar]

            // Check AST for semantic errors
            initTree (grammar)
            val messages = analyser.errors (grammar)

            if (messages.length == 0) {

                // Make a desugarer for this run
                val desugarer = new Desugarer (analyser)

                // No errors, go on to desugaring, translation and generation
                val desugaredGrammar = desugarer.desugar (grammar)
                resetMemo ()
                initTree (desugaredGrammar)

                // Make a translator for this run
                val translator = new Translator (analyser)

                // Extract basename and package directories from grammar name
                val basename = grammar.module.last
                val genSubDir = grammar.module.init.foldLeft (genDir) (_ / _)
                val outSubDir = grammar.module.init.foldLeft (outDir) (_ / _)

                // Generate the Rats! specification
                val genFile = genSubDir / s"${basename}.rats"
                str.log.info (s"Syntax generating Rats! file $genFile")
                translator.translate (flags, genFile, desugaredGrammar)

                // Buffer of extra generated files
                val extraFiles = ListBuffer[File] ()

                // If requested, generate the AST classes
                if (flags.defineASTClasses) {
                    val astFile = outSubDir / s"${basename}Syntax.scala"
                    str.log.info (s"Syntax generating AST classes $astFile")
                    generator.generateASTClasses (flags, astFile, grammar)
                    extraFiles.append (astFile)
                }

                // If requested, generate the AST classes
                if (flags.defineASTClasses && flags.definePrettyPrinter) {
                    val ppFile = outSubDir / s"${basename}PrettyPrinter.scala"
                    str.log.info (s"Syntax generating pretty-printer $ppFile")
                    generator.generatePrettyPrinter (flags, ppFile, grammar)
                    extraFiles.append (ppFile)
                }

                Some ((outSubDir, genFile, extraFiles.result ()))

            } else {

                for (message <- sortmessages (messages))
                    str.log.error (formatSemanticError (p, filename, message))
                sys.error (s"Syntax semantic analysis of $main failed")

            }

        } else {

            str.log.error (formatParseError (p, pr.parseError))
            sys.error (s"Syntax parsing $main failed")

        }
    }

    /**
     * Format a Rats! parser error message according to Scala compiler
     * conventions for better compatibility with error processors (e.g.,
     * editors).  Very similar to `p.format` but omits the column
     * number from the first line since the pointer carries that
     * information, and omits the word "error".
     */
    def formatParseError (p : Parser, error : ParseError) : String = {
        val buf = new StringBuilder

        if (error.index == -1)
            buf.append (error.msg)
        else {
            val loc = p.location (error.index)
            buf.append (loc.file)
            buf.append (':')
            buf.append (loc.line)
            buf.append (": ")

            buf.append (error.msg)
            buf.append ('\n')

            val line = p.lineAt (error.index)
            buf.append (line)
            buf.append ('\n')
            buf.append (" " * (loc.column - 1))
            buf.append ("^\n")
        }

        buf.result ()
    }

    /**
     * Format a semantc error according to Scala compiler conventions.
     */
    def formatSemanticError (p : Parser, filename : String, message : Message) : String = {
        val pos = message.pos.asInstanceOf[LineColPosition]
        s"$filename:${pos.line}: ${message.label}\n${p.lineAt (pos.index)}\n${(" " * (pos.column - 1))}^"
    }

    /**
     * Run Rats! on the `main` file.
     */
    def runRatsImpl (flags : Flags, main : File, isUserMain : Boolean,
                     srcDir : File, genDir : File, outDir : File,
                     str : TaskStreams) : Set[File] = {

        // Set up paths and output directories
        val mainPath = main.absolutePath
        val mainDir = main.getParentFile
        val ratsOutDir =
            if (isUserMain) {
                val relFile = main.getParent.drop (srcDir.getPath.length)
                val ratsGenDir = genDir / relFile
                if (ratsGenDir.exists () || ratsGenDir.mkdirs ())
                    ratsGenDir
                else
                    sys.error (s"Can't create Rats! output dir $ratsGenDir")
            } else
                main.getParentFile

        /**
         * Version of Rats! runtime so we can customise the error message reporting
         * to be in stanard Scala style.
         */
        class RatsRuntime extends Runtime {

            def loc (n : Node) : String = {
                val loc = n.getLocation
                if (loc == null)
                    "unknown location: "
                else
                    s"${loc.file}:${loc.line}: "
            }

            override def error (msg : String) {
                str.log.error (msg)
                errors = errors + 1
            }

            override def error (msg : String, n : Node) {
                error (loc (n) + msg)
            }

            override def warning (msg : String) {
                str.log.warn (msg)
                warnings = warnings + 1
            }

            override def warning (msg : String, n : Node) {
                warning (loc (n) + msg)
            }

        }

        /*
         * The runtime field of Rats! tool can't be overridden because it's
         * protectted final. So hack in there using reflection so we can
         * customise it.
         */
        def overrideRuntime (rats : RatsRunner) : RatsRunner = {
            val toolClass = rats.getClass.getSuperclass.getSuperclass
            val runtimeField = toolClass.getDeclaredField ("runtime")
            runtimeField.setAccessible (true)
            runtimeField.set (rats, new RatsRuntime)
            rats
        }

        /*
         * Helper class to enable access to the protected runtime field.
         */
        class RatsRunner extends Rats {
            def getRuntime = runtime
        }

        // Make a customised Rats! runner
        val rats = overrideRuntime (new RatsRunner)

        // Actually run Rats!
        str.log.info (s"Running Rats! on $mainPath, input from $mainDir and $genDir, output to $ratsOutDir")
        rats.run (Array ("-silent", "-no-exit",
                         "-in", mainDir.absolutePath,
                         "-in", genDir.absolutePath,
                         "-out", ratsOutDir.absolutePath,
                         mainPath))

        // What happened?
        if (rats.getRuntime.seenError) {

            sys.error ("Rats! failed")
            Set.empty

        } else {
            // Get the expected generated file
            val basename = main.getName.takeWhile (_ != '.')
            val basenameext = s"${basename}.java"
            val genFile = ratsOutDir / basenameext

            // If we've got it, process it further
            if (genFile.exists) {
                str.log.info (s"Rats! generated $genFile")

                val relFile = genFile.getPath.drop (genDir.getPath.length)
                val outFile = outDir / relFile

                if (flags.scalaRepetitionType != None || flags.useScalaOptions || flags.useScalaPositions) {

                    str.log.info (s"Rats! transforming $genFile for Scala into $outFile")
                    transformForScala (flags, genFile, outFile)
                    Set (outFile)

                } else {

                    str.log.info (s"Rats! copying $genFile to $outFile")
                    IO.copyFile (genFile, outFile, true)
                    Set (outFile)

                }
            } else {

                sys.error (s"Rats!, can't find generated file $genFile")
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
     *  - scalaRepetitionType != None: replace xtc pairs with specified Scala type
     *  - useScalaPositions: replace Rats! location code, gen LineColPosition class
     */
    def transformForScala (flags : Flags, genFile : File, outFile : File) {

        def transformPairsToSequences (seqType : String, contents : String) : String = {
            val pairsToSequences =
                List (
                    """import xtc\.util\.Pair;""".r ->
                        s"""import xtc.util.Pair;
                           |import scala.collection.immutable.$seqType;
                           |import sbtrats.ParserSupport;
                           |import sbtrats.S$seqType;""".stripMargin,
                    """(Pair<.*>) (v[0-9]+) = ([^.]+)\.reverse\(\);""".r ->
                        s"""$$1 $$2 = S$seqType.reverse($$3);""",
                    """Pair\.empty\(\)""".r ->
                        s"""S$seqType.empty()""",
                    """new Pair<.*>\(""".r ->
                        s"""S$seqType.create(""",
                    """Pair<Pair(<[^>]+>)>""".r ->
                        s"""$seqType<$seqType$$1>""",
                    """Pair(<[^>]+>)""".r ->
                        s"""$seqType$$1"""
                )
            makeReplacements (contents, pairsToSequences)
        }

        def transformNullablesToOptions (contents : String) : String = {
            val nullablesToOptions =
                List (
                    """import xtc\.util\.Pair;""".r ->
                        """import xtc.util.Pair;
                          |import scala.Option;""".stripMargin
                )
            makeReplacements (contents, nullablesToOptions)
        }

        def transformPositions (contents : String) : String = {

            val locatablesToPositionsScala =
                List (
                    """import xtc\.tree\.Locatable;""".r ->
                        "",
                    """import xtc\.tree\.Location;""".r ->
                        """import xtc\.tree\.Location;
                          |import scala.util.parsing.input.Positional;
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
                        |      positional.setPos(new LineColPosition(this, index, c.line, c.column));
                        |    }
                        |  }
                        |
                        |  /** Set the position of a Positional to be the same as another one */
                        |  void copyLocation(final Positional positional, final Positional source) {
                        |    if ((null != positional) && (null != source)) {
                        |      setLocation(positional, source.getPos.index);
                        |    }
                        |  }
                        |""".stripMargin
                )

            val locatablesToPositionsKiama1 =
                List (
                    """import xtc\.tree\.Locatable;""".r ->
                        "",
                    """import xtc\.tree\.Location;""".r ->
                        """import xtc\.tree\.Location;
                          |import org.kiama.util.Positions;
                          |import scala.util.parsing.input.Position;
                          |import sbtrats.LineColPosition;""".stripMargin,
                    """Locatable""".r ->
                        """Object""",
                    """public final class (\w+) extends ParserBase \{""".r ->
                        """
                        |public final class $1 extends ParserBase {
                        |
                        |  /** Last result before a location is set. */
                        |  protected Result yyLastResult;
                        |
                        |  public Boolean isSpace(int c) {
                        |    return (c == ' ') || (c == '\\f') || (c == '\\n') || (c == '\\r') || (c == '\\t');
                        |  }
                        |
                        |  /**
                        |   * Return the first previous non-whitespace character index before
                        |   * the given index. If there are no such characters return 0.
                        |   */
                        |  int prevNonWS(int index) {
                        |    while (index > 0 && Character.isWhitespace(yyData[index-1]))
                        |      index--;
                        |    return index;
                        |  }
                        |
                        |  /** Set start position of an Object to a start index and the finish
                        |   *  position to the current parsing index. */
                        |  void setLocation(final Object object, final int start) {
                        |    if (null != object) {
                        |      Column s = column(start);
                        |      Positions.setStart(object, new LineColPosition(this, start, s.line, s.column));
                        |      int finish = prevNonWS(yyLastResult.index);
                        |      Column f = column(finish);
                        |      Positions.setStart(object, new LineColPosition(this, finish, f.line, f.column));
                        |    }
                        |  }
                        |
                        |  /** Set the start position of an Object to the start location of
                        |   *  another one and the finish position to the current parsing index. */
                        |  void copyLocation(final Object object, final Object source) {
                        |    if ((null != object) && (null != source)) {
                        |      setLocation(object, ((LineColPosition)Positions.getStart(source)).index());
                        |    }
                        |  }
                        |""".stripMargin
                )

            val locatablesToPositionsKiama2 =
                List (
                    """import xtc\.tree\.Locatable;""".r ->
                        "",
                    """import xtc\.tree\.Location;""".r ->
                        """import xtc\.tree\.Location;
                          |import org.bitbucket.inkytonik.kiama.parsing.Input;
                          |import org.bitbucket.inkytonik.kiama.util.FileSource;
                          |import org.bitbucket.inkytonik.kiama.util.Message;
                          |import org.bitbucket.inkytonik.kiama.util.Position;
                          |import org.bitbucket.inkytonik.kiama.util.Positions;
                          |import org.bitbucket.inkytonik.kiama.util.Source;""".stripMargin,
                    """Locatable""".r ->
                        """Object""",
                    """public final class (\w+) extends ParserBase \{""".r ->
                        """
                        |public final class $1 extends ParserBase {
                        |
                        |  // =========================================================================
                        |
                        |  /** The Kiama source from which input is being read. */
                        |  protected Source source;
                        |
                        |  /** The Kiama position store being used to track value positions. */
                        |  protected Positions positions;
                        |
                        |  /** Last result before a location is set. */
                        |  protected Result yyLastResult;
                        |
                        |  /**
                        |   * Create a new packrat parser.
                        |   *
                        |   * @param src The Kiama source to be parsed.
                        |   * @param posns The store in which to keep track of parsed value positions.
                        |   * @throws NullPointerException Signals a null file name.
                        |   * @throws IllegalArgumentException Signals a negative file size.
                        |   */
                        |  public $1(final Source src, final Positions posns) {
                        |      this(src, INIT_SIZE - 1, posns);
                        |  }
                        |
                        |  /**
                        |   * Create a new packrat parser.
                        |   *
                        |   * @param src The Kiama source to be parsed.
                        |   * @param size The length of the character stream.
                        |   * @param posns The store in which to keep track of parsed value positions.
                        |   * @throws NullPointerException Signals a null file name.
                        |   * @throws IllegalArgumentException Signals a negative file size.
                        |   */
                        |  public $1(final Source src, final int size, final Positions posns) {
                        |      this(src.reader(), src.name(), size);
                        |      source = src;
                        |      positions = posns;
                        |  }
                        |
                        |  /**
                        |   * Return the first previous non-whitespace character index before
                        |   * the given index. If there are no such characters return 0.
                        |   */
                        |  int prevNonWS(int index) {
                        |    while (index > 0 && Character.isWhitespace(yyData[index-1]))
                        |      index--;
                        |    return index;
                        |  }
                        |
                        |  /** Set start position of an ooject to one that corresponds to a start
                        |   *  index and the finish position to the one that corresponds to the
                        |   *  first non-whitespace character before the current parsing index.
                        |   *  Nothing is done if the object already has a start position.
                        |   */
                        |  void setLocation(final Object object, final int start) {
                        |    if (null != object) {
                        |      scala.Option<Position> optStart = positions.getStart(object);
                        |      if (optStart.isEmpty()) {
                        |        Column s = column(start);
                        |        positions.setStart(object, new Position(s.line, s.column, source));
                        |        int finish = prevNonWS(yyLastResult.index);
                        |        Column f = column(finish);
                        |        positions.setFinish(object, new Position(f.line, f.column, source));
                        |      }
                        |    }
                        |  }
                        |
                        |  /** Set the start of an object to the start of another object. If the
                        |   *  source object doesn't have a start position, do nothing. Also, set
                        |   *  the finish position to the one that corresponds to the first
                        |   *  non-whitespace character before the current parsing index.
                        |   */
                        |  void copyLocation(final Object object, final Object another) {
                        |    if ((null != object) && (null != another)) {
                        |      scala.Option<Position> optStart = positions.getStart(another);
                        |      if (!optStart.isEmpty()) {
                        |        scala.Option<Object> optOffset = optStart.get().optOffset();
                        |        if (!optOffset.isEmpty())
                        |          setLocation(object, ((Integer)optOffset.get()).intValue());
                        |      }
                        |    }
                        |  }
                        |
                        |  /** Return a Kiama message for a given parse error. */
                        |  public Message errorToMessage(ParseError error) {
                        |    Location loc = location(error.index);
                        |    Position pos = new Position(loc.line, loc.column, source);
                        |    positions.setStart(error, pos);
                        |    positions.setFinish(error, pos);
                        |    return new Message(error, error.msg);
                        |  }
                        |
                        |  /** Return a Kiama input for a given parse index. */
                        |  public Input indexToInput(int index) {
                        |    return new Input(source, index);
                        |  }
                        |""".stripMargin
                )

            makeReplacements (contents, flags.useKiama match {
                                            case 1 => locatablesToPositionsKiama1
                                            case 2 => locatablesToPositionsKiama2
                                            case _ => locatablesToPositionsScala
                                        })

        }

        def transformMessages (contents : String) : String = {
            val removeLevel0InMessage =
                List (
                    " level[0-9]+ expected\"".r ->
                        " expected\""
                )
            makeReplacements (contents, removeLevel0InMessage)
        }

        val contents = IO.read (genFile)

        val contents1 =
            flags.scalaRepetitionType match {
                case Some (ListType) =>
                    transformPairsToSequences ("List", contents)
                case Some (VectorType) =>
                    transformPairsToSequences ("Vector", contents)
                case None =>
                    contents
            }

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

        val contents4 =
            transformMessages (contents3)

        IO.write (outFile, contents4)

    }

    /**
     * Settings for the plugin:
     *  - run Rats! as a source generator
     *  - add the Rats! jar to the dependent libraries
     *  - default values for settings
     *  - group settings together to pass around
     */
    override lazy val projectSettings = Seq (

        sourceGenerators in Compile += runGenerators,

        libraryDependencies ++= Seq (
            "xtc" % "rats" % "2.3.1"
        ),

        ratsMainModule := None,

        ratsScalaRepetitionType := None,

        ratsUseScalaPositions := false,

        ratsUseScalaOptions := false,

        ratsUseDefaultComments := true,

        ratsUseDefaultSpacing := true,

        ratsUseDefaultLayout := true,

        ratsUseDefaultWords := true,

        ratsDefineASTClasses := false,

        ratsDefinePrettyPrinter := false,

        ratsPrecomputeHashCodes := false,

        ratsIncludeKeywordTable := true,

        ratsIncludeBinarySupport := false,

        ratsUseKiama := 0,

        ratsFlags := {
            val kiamaPkg = if (ratsUseKiama.value < 2) "org.kiama" else "org.bitbucket.inkytonik.kiama"
            Flags (ratsScalaRepetitionType.value, ratsUseScalaPositions.value,
                   ratsUseScalaOptions.value, ratsUseDefaultComments.value,
                   ratsUseDefaultSpacing.value, ratsUseDefaultLayout.value,
                   ratsUseDefaultWords.value, ratsDefineASTClasses.value,
                   ratsDefinePrettyPrinter.value, ratsPrecomputeHashCodes.value,
                   ratsIncludeKeywordTable.value, ratsIncludeBinarySupport.value,
                   ratsUseKiama.value, kiamaPkg)
        }

    )

}
