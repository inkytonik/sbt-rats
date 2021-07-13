/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012-2020 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

import org.kiama.output.PrettyPrinter

/**
 * Translator to Rats! specifications, parameteriesed by the sementic
 * analyser to use.
 */
class Translator (analyser : Analyser) extends PrettyPrinter {

    import ast._
    import sbt.File
    import sbt.IO.write

    def translate (flags : Flags, genFile : File, grammar : Grammar) = {

        import analyser.{constr, elemtype, hasSpacing, hasValue, isTransient, ntname,
            nttype, partitionLiterals, requiresNoAction, syntaxElements,
            transformer, typeName, voidType}
        import org.kiama.attribution.Attribution.{initTree, resetMemo}

        // Count of non-terminals on the RHS of an alternative
        // FIXME: remove or hide better
        var ntcount = 0

        def toRats : Doc = {
            val (keywords, symbols) = partitionLiterals (grammar)
            "module" <+> grammar.module.mkString (".") <> semi <@>
            toHeader (grammar.header) <@>
            toBody (grammar.body, keywords) <@>
            toOptions (grammar.options) <@>
            toSymbols (symbols) <@>
            toRules (grammar.rules) <@>
            toDefaults
        }


        def toHeader (header : String) : Doc = {
            val actionType : String =
                if (flags.scalaRepetitionType.isDefined)
                    "sbtrats.Action"
                else
                    "xtc.util.Action"

            line <>
            toBraceSection ("header",
                s"import $actionType;" <@>
                "import xtc.tree.Location;",
                if (header == null) empty else string (header)
            )
        }

        def toBody (userBody : String, keywords : Set[Literal]) : Doc = {

            lazy val filenameValue =
                if ((grammar.options != null) && (grammar.options.contains (RelativeFilenames ())))
                    "dropCurrentPath (loc.file)"
                else
                    "loc.file"

            lazy val fixedBody =
                s"""
                |/**
                | * Drop the current path off string when it occurs at the beginning.
                | */
                |public String dropCurrentPath (String string) {
                |    int index = 0;
                |    int stringlen = string.length ();
                |    String prefix = System.getProperty (\"user.dir\");
                |    int prefixlen = prefix.length ();
                |    while ((index < stringlen) && (index < prefixlen) && (string.charAt (index) == prefix.charAt (index))) {
                |        index++;
                |    }
                |    if ((index != 0) && (string.charAt (index) == java.io.File.separatorChar)) {
                |        index++;
                |    }
                |    return string.substring (index);
                |}
                |
                |/**
                | * Format a Rats! parser error message according to Scala compiler
                | * conventions for better compatibility with error processors.
                | */
                |public String formatParseError (ParseError error, Boolean showCoord) throws IOException {
                |    StringBuilder buf = new StringBuilder ();
                |
                |    if (error.index == -1) {
                |        buf.append (error.msg);
                |    } else {
                |        Location loc = location (error.index);
                |        if (showCoord) {
                |            String filename = $filenameValue;
                |            buf.append (filename);
                |            buf.append (':');
                |            buf.append (loc.line);
                |            buf.append (": ");
                |        }
                |
                |        buf.append (error.msg);
                |        buf.append ("\\n");
                |
                |        String line = lineAt (error.index);
                |        buf.append (line);
                |        buf.append ('\\n');
                |        for (int i = 1; i < loc.column; i++) buf.append (' ');
                |        buf.append ("^");
                |    }
                |
                |    return buf.toString ();
                |}
                |
                |/** Skip forward n characters in the input. */
                |public void skip(int n) throws IOException {
                |  for (int i = 0; i < n; i++)
                |    character(i);
                |}
                """.stripMargin

            lazy val binarySupport =
                """
                |int strToInt (String number) {
                |    try {
                |        byte[] bytes = number.getBytes ("ISO-8859-1");
                |        return new java.math.BigInteger (1, bytes).intValue ();
                |    } catch (java.io.UnsupportedEncodingException e) {
                |        System.err.println ("strToInt: unsupported encoding exception");
                |        return 0;
                |    }
                |}
                |
                |Result parseBytes (String number, int start, int base) throws IOException {
                |    int n = strToInt (number);
                |    StringBuilder buf = new StringBuilder (n);
                |    for (int i = 0; i < n; i++) {
                |        int c = character (base + i);
                |        if (c != -1) {
                |            buf.append ((char)c);
                |        } else {
                |            return new ParseError (String.format ("expected %d bytes but EOF found", n), base + i);
                |        }
                |    }
                |    return new SemanticValue (buf.toString (), base + n);
                |}
                |""".stripMargin

            lazy val keywordTable =
                toBraceSection ("static",
                    group (toKeywords (keywords))
                )

            val possibleBodyParts =
                List (true                       -> text (fixedBody),
                      (userBody != null)         -> text (userBody),
                      flags.includeBinarySupport -> text (binarySupport),
                      flags.includeKeywordTable  -> keywordTable)

            val bodyParts =
                possibleBodyParts.filter (_._1).map (_._2)

            bodyParts match {
                case Nil =>
                    empty
                case _ =>
                    line <>
                    toBraceSection ("body", vsep (bodyParts))
            }

        }

        def toBraceSection (keyword : String, contents : Doc, prefix : Doc = empty) : Doc =
            keyword <+> braces (prefix <> nest (line <> contents) <> line)

        def toKeywords (keywords : Set[Literal]) : Doc =
            "add" <+> parens (
                toBraceSection ("KEYWORDS, new String[]",
                    fillsep (keywords.map(_.s).toVector.sorted map (s => dquotes (text (s))), comma)
                )
            ) <> semi

        def toOptions (options : List[SyntaxOption]) : Doc = {
            val userOptionStrings =
                if (options == null)
                    Nil
                else
                    options.distinct.flatMap {
                        case Verbose ()      => List ("verbose")
                        case SetOfString (s) => List (s"setOfString ($s)")
                        case _               => Nil
                    }
            val possibleOptions =
                List (flags.includeKeywordTable -> "setOfString (KEYWORDS)",
                      flags.useScalaPositions   -> "withLocation")
            val optionStrings =
                userOptionStrings ++
                possibleOptions.filter (_._1).map (_._2)
            line <>
            "option" <+> hsep (optionStrings map text, comma) <> semi
        }

        def escapedDquotes (lit : Literal) : Doc =
            dquotes (lit.escaped)

        def toSymbols (symbols : Set[Literal]) : Doc =
            if (symbols.isEmpty)
                empty
            else {
                // Define SymbolN for each length N of symbol literals
                symbols.groupBy (_.ss.length).foldLeft (empty) {
                    case (d, (l, ss)) =>
                        val uniqLits = ss.map (_.escaped).toVector
                        d <@>
                        line <>
                        s"String Symbol$l =" <>
                        nest (
                            line <>
                            s"Symbol${l}Alts Spacing;"
                        ) <@>
                        line <>
                        s"transient String Symbol${l}Alts =" <>
                        nest (
                            line <>
                            fillsep (uniqLits.map (s => dquotes (text (s))), " /")
                        ) <> semi
                }
            }

        def toRules (rules : List[Rule]) : Doc =
            vsep (rules map toRule)

        def toRule (rule : Rule) =
            rule match {
                case r : ASTRule    => toASTRule (r)
                case r : StringRule => toStringRule (r)
                case r : RatsRule   => toRatsRule (r)
                case RatsBlock (c)  => string (c.mkString)
            }

        def toRHS (elem : Element, isASTRule : Boolean, useSpacing : Boolean) : Doc = {

            def isWord (s : String) : Boolean =
                (s == "") ||
                  (Character.isJavaIdentifierStart (s.head) &&
                   s.tail.forall (Character.isJavaIdentifierPart))

            def toLiteral (lit : Literal) : Doc = {
                val prefix : Doc = if (isASTRule) "void" <> colon else ""
                val form = if (isWord (lit.s)) "Word" else s"Symbol${lit.ss.length}"
                val suffix = if (useSpacing) colon <> form else empty
                prefix <> escapedDquotes (lit) <> suffix
            }

            def toElem (elem : Element, doBindings : Boolean = false) : Doc = {

                def bind (doc : Doc) : Doc =
                    if (doBindings) {
                        ntcount = ntcount + 1
                        "v" <> value (ntcount) <> colon <> doc
                    } else
                        doc

                def action (d : Doc) : Doc =
                    "^" <> braces (nest (line <> d) <> line)

                def toBlock (n : Int) : Doc = {
                    ntcount = ntcount + 1
                    action (
                        "String v" <> value (ntcount) <> semi <>
                        line <>
                        "yyResult = parseBytes(v" <> value (n) <> ", yyStart, yyBase);" <>
                        line <>
                        "if (yyResult.hasValue()) {" <>
                        nest (
                            line <>
                            "v" <> value (ntcount) <+> "= yyResult.semanticValue();" <>
                            line <>
                            "yyResult = new SemanticValue(null, yyResult.index);"
                        ) <>
                        line <>
                        "} else {" <>
                        nest (
                            line <>
                            "v" <> value (ntcount) <+> "= null;"
                        ) <>
                        line <>
                        "}"
                    )
                }

                elem match {
                    case nt : NonTerminal =>
                        if (nt->nttype == voidType)
                            text (nt->ntname)
                        else
                            bind (text (nt->ntname))

                    case And (elem) =>
                        "&" <> parens (toElem (elem))

                    case Not (elem) =>
                        "!" <> parens (toElem (elem))

                    case Opt (elem) =>
                        val inner = parens (toElem (elem)) <> "?"
                        if (elem->hasValue)
                            bind (inner)
                        else
                            inner

                    case Rep (zero, elem, Epsilon ()) =>
                        val inner = parens (toElem (elem)) <> (if (zero) "*" else "+")
                        if (elem->hasValue)
                            bind (inner)
                        else
                            inner
                    case _ : Rep =>
                        sys.error ("toElem: separated list left in for translation")

                    case Alt (left, right) =>
                        parens (toElem (left) <> "/" <> toElem (right))

                    case Seqn (left, right) =>
                        val inner = toElem (left) <+> toElem (right)
                        if (elem->hasValue)
                            bind (inner)
                        else
                            inner

                    case CharLit (lit) =>
                        toLiteral (lit)
                    case StringLit (lit) =>
                        toLiteral (lit)

                    case CharClass (str) =>
                        brackets (str)

                    case Epsilon () =>
                        "/* empty */"
                    case Wildcard () =>
                        "_"

                    case Nest (elem, _) =>
                        toElem (elem, doBindings)

                    case Block (_, n) =>
                        toBlock (n)

                    // Formatting directives
                    case _ =>
                        empty
                }

            }

            toElem (elem, isASTRule)

        }

        def toASTRule (astRule : ASTRule) : Doc = {

            val ASTRule (lhs, declType, alts, isConst, _) = astRule

            def toAlternative (a : Alternative) : Doc = {
                ntcount = 0

                // We want to use spacing between symbols by default but not if
                // there is a "nospacing" annotation on the rule
                val useSpacing = astRule->hasSpacing

                hsep (a.rhs map (elem => toRHS (elem, true, useSpacing))) <+> toAction (a)
            }

            def toAction (alt : Alternative) : Doc = {

                // Get the syntax elements of this alternative
                val elements = alt->syntaxElements

                // The arguments are v1 .. vn. Normally these are passed straight through
                // to the constructor. However, if there is an annotation of the form
                // n:m then the value passed through for argument n is the result of
                // passing the original value to the method m. If an argument is an option
                // and we are using Scala options, then we translate from the nullable
                // representation to the Scala option value as we pass the argument.
                val argList =
                    (1 to ntcount).map {
                        case n =>
                            val argName = "v" <> value (n)
                            val argExp =
                                (elements (n - 1)->elemtype) match {
                                    case _ : OptionType if flags.useScalaOptions =>
                                        "Option.apply" <+> parens (argName)
                                    case _ =>
                                        argName
                                }
                            (alt->transformer (n) match {
                                case Some (method) =>
                                    method <+> parens (argExp)
                                case None =>
                                    argExp
                             })
                    }

                // Pretty-printed argument list for constructor
                val args = hsep (argList, comma)

                if (alt->requiresNoAction)
                    empty
                else
                    braces (nest (line <>
                        "yyLastResult = yyResult;" <> line <>
                        "yyValue" <+> equal <+>
                            (alt.action match {
                                case ApplyAction () =>
                                    if (flags.scalaRepetitionType.isDefined)
                                        "ParserSupport.apply(v2, v1)"
                                    else
                                        "apply(v2, v1)"
                                case ConsAction (tipe) =>
                                    s"new Pair<$tipe>(v1, v2)"
                                case DefaultAction () =>
                                    "new" <+> text (alt->constr) <+> parens (args)
                                case NoAction () =>
                                    // Not reachable
                                    empty
                                case NilAction () =>
                                    "Pair.empty()"
                                case SingletonAction (tipe) =>
                                    s"new Pair<$tipe>(v1)"
                                case TailAction (tipe, constr) =>
                                    val num = elements.length
                                    val args =
                                        if (num == 0)
                                            ""
                                        else {
                                            elements.zipWithIndex.map {
                                                case (Opt(_), n) =>
                                                    s"Option.apply (v${n+1})"
                                                case (_, n) =>
                                                    s"v${n+1}"
                                            }.mkString (", ", ", ", "")
                                        }
                                    toBraceSection (s"new Action<$tipe>()",
                                        toBraceSection (s"public $tipe run ($tipe left)",
                                            constr <+> "node" <+> "=" <+> "new" <+> constr <+> parens ("left" <> args) <> semi <@>
                                            (if (flags.useScalaPositions)
                                                "copyLocation(node, left);" <> line
                                             else
                                                 empty) <>
                                            "return node;"
                                        ) <> semi
                                    )
                             }) <> semi <>
                        (alt.action match {
                            case _ : ConsAction | _ : SingletonAction =>
                                line <>
                                "setLocation(yyValue, yyStart);"
                            case NilAction () =>
                                line <>
                                "setLocation(yyValue, yyCount);"
                            case _ =>
                                empty
                         })
                    ) <> line)

            }

            line <>
            (if (isConst) "constant" else "public") <+>
            ruleModifier(astRule) <+>
            text (astRule->typeName) <+> lhs.name <+> equal <>
            nest (
                line <>
                lsep2 (alts map toAlternative, "/") <> semi
            )

        }

        def toStringRule (stringRule : StringRule) : Doc = {

            val StringRule (lhs, IdnUse (typeName), alts, _) = stringRule

            val modifier = ruleModifier(stringRule)
            val tipe = implTypeName(typeName)

            def toAlternative (e : Element) : Doc =
                toRHS (e, false, false)

            def auxname (name : String) : String =
                name + "Form"

            def auxdef (name : String) =
                line <>
                "public" <+> modifier <+> "String" <+> name <+> equal <>
                nest (
                    line <>
                    auxname (name) <+> "Spacing" <@>
                    semi
                )

            def maindef (name : String) : Doc =
                line <>
                "public" <+> modifier <+> tipe <+> name <+> equal <>
                nest (
                    line <>
                    lsep2 (alts map toAlternative, "/") <> semi
                )

            if (typeName == "Token")
                auxdef (lhs.name) <@> maindef (auxname (lhs.name))
            else
                maindef (lhs.name)

        }

        def implTypeName(tipe : String) : String =
            tipe match {
                case "Void"     => "void"
                case "Token"    => "String"
                case name       => name
            }

        def toRatsRule (ratsRule : RatsRule) : Doc = {

            val RatsRule (lhs, tipe, code, _) = ratsRule

            val spacing =
                if (tipe.name == "Token")
                    space <> text("Spacing")
                else
                    empty

            line <>
            "public" <+> ruleModifier(ratsRule) <+> implTypeName(tipe.name) <+> lhs.name <+> equal <>
            code <> spacing <> semi

        }

        def ruleModifier(rule: Rule): String =
            if (rule->isTransient) "transient" else ""

        /*
         * Generate default implementations of various aspects, depending on the
         * flag settings.
         */
        def toDefaults : Doc = {

            val spacingSpec =
                if (flags.useDefaultSpacing)
                    """
                    |// Default spacing specification
                    |public transient void Spacing   = (Space / Comment)*;
                    |""".stripMargin
                else
                    ""

            val layoutSpec =
                if (flags.useDefaultLayout)
                    """
                    |// Default layout specification
                    |public transient void Space     = ' ' / '\t' / '\f' / EOL;
                    |public transient void EOL       = '\r' '\n' / '\r' / '\n';
                    |""".stripMargin
                else
                    ""

            val commentSpec =
                if (flags.useDefaultComments)
                    """
                    |// Default comment specification
                    |public transient void Comment   = SLComment / MLComment;
                    |public transient void SLComment = "//" (!EOL _)* (EOL / EOF);
                    |public transient void MLComment = "/*" (MLComment / !"*/" _)* "*/";
                    |""".stripMargin
                else
                    ""

            val wordsSpec =
                if (flags.useDefaultWords)
                    """
                    |// Default word specification
                    |public String Identifier =
                    |    Word &{
                    |        ! contains (KEYWORDS, yyValue)
                    |    };
                    |
                    |public String Word =
                    |    WordCharacters Spacing;
                    |
                    |public transient String WordCharacters =
                    |    h:_ &{
                    |        Character.isJavaIdentifierStart (h)
                    |    } (t:_ &{
                    |        Character.isJavaIdentifierPart (t)
                    |    })*;
                    |""".stripMargin
                else
                    ""

            string (spacingSpec) <@>
            string (layoutSpec) <@>
            string (commentSpec) <@>
            string (wordsSpec) <@>
            "public transient void EOF      = !_;"

        }

        // Initialise the tree so we can perform attribution on it
        resetMemo ()
        initTree (grammar)

        // Convert grammar to a pretty printer document representing the translated
        // Rats! specification
        val code = pretty (toRats)

        // Put the code in the specified file
        write (genFile, code)

    }

}
