/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

import org.kiama.output.PrettyPrinter

object Translator extends PrettyPrinter {
    
    import ast._
    import sbt.File
    import sbt.IO.write

    def translate (flags : Flags, genFile : File, grammar : Grammar) = {

        import Analyser.{constr, partitionLiterals, requiresNoAction, transformer,
            typeName}
        import org.kiama.attribution.Attribution.initTree

        // Count of non-terminals on the RHS of an alternative
        // FIXME: remove or hide better
        var ntcount = 0

        def toRats : Doc = {
            val (keywords, symbols) = partitionLiterals (grammar)
            "module" <+> grammar.pkg.mkString (".") <> semi <@>
            toHeader (grammar.header, keywords) <@>
            toOptions <@>
            toSymbols (symbols) <@>
            toRules (grammar.rules) <@>
            toDefaults
        }

        def toHeader (header : String, keywords : Set[String]) : Doc =
            line <>
            toBraceSection ("header",
                "import sbtrats.Action;",
                if (header == null) empty else string (header)
            ) <@>
            line <>
            toBraceSection ("body",
                toBraceSection ("static",
                    group (toKeywords (keywords))
                )
            ) 

        def toBraceSection (keyword : String, contents : Doc, prefix : Doc = empty) : Doc =
            keyword <+> braces (prefix <> nest (line <> contents) <> line)

        def toKeywords (keywords : Set[String]) : Doc =
            "add" <+> parens (
                toBraceSection ("KEYWORDS, new String[]",
                    fillsep (keywords.toSeq.sorted map (s => dquotes (text (s))), comma)
                )
            ) <> semi

        def toOptions : Doc = {
            val options =
                "setOfString (KEYWORDS)" ::
                    (if (flags.useScalaPositions)
                         List ("withLocation")
                     else
                         Nil)

            line <>
            "option" <+> hsep (options map text, comma) <> semi
        }

        def toSymbols (symbols : Set[String]) : Doc = {

            // Sort in reverse order of length so that earlier ones take priority
            val sortedSymbols = symbols.toSeq.sortBy (- _.length)

            line <>
            "String Symbol =" <>
            nest (
                line <>
                "SymbolCharacters Spacing;"
            ) <@>
            line <>
            "transient String SymbolCharacters =" <>
            nest (
                line <>
                fillsep (sortedSymbols map (dquotes (_)), " /")
            ) <> semi

        }

        def toRules (rules : List[Rule]) : Doc =
            vsep (rules map toRule)

        def toRule (rule : Rule) =
            rule match {
                case r : ASTRule     => toASTRule (r)
                case r : StringRule  => toStringRule (r)
                case RatsSection (c) => string (c.mkString)
            }

        def toRHS (elem : Element, isASTRule : Boolean) : Doc = {

            def toLiteral (s : String) : Doc = {
                val prefix : Doc = if (isASTRule) "void" <> colon else ""
                val form = if (s.forall (_.isLetter)) "Word" else "Symbol"
                val suffix = if (isASTRule) colon <> form else empty
                prefix <> dquotes (s) <> suffix
            }

            def toElem (elem : Element, doBindings : Boolean = false) : Doc = {

                def bind (doc : Doc) : Doc =
                    if (doBindings) {
                        ntcount = ntcount + 1
                        "v" <> value (ntcount) <> colon <> doc
                    } else
                        doc

                elem match {
                    case NonTerminal (IdnUse (i))  => bind (i)

                    case Not (elem)                => "!" <> parens (toElem (elem))
                    case Opt (elem)                => bind (parens (toElem (elem)) <> "?")
                    case Rep (zero, elem)          => bind (parens (toElem (elem)) <>
                                                                (if (zero) "*" else "+"))

                    case Alt (left, right)         => parens (toElem (left) <> "/" <>
                                                                  toElem (right))
                    case Seqn (left, right)        => toElem (left) <+> toElem (right)

                    case CharLit (str)             => toLiteral (str)
                    case StringLit (str)           => toLiteral (str)

                    case CharClass (str)           => brackets (str)

                    case Epsilon ()                => "/* empty */"
                    case Wildcard ()               => "_"

                    case Nest (elem)               => toElem (elem, doBindings)

                    case _                         => empty
                }

            }

            toElem (elem, isASTRule)

        }

        def toASTRule (astRule : ASTRule) : Doc = {

            val ASTRule (lhs, declType, alts, isConst, _) = astRule

            def toAlternative (a : Alternative) : Doc = {
                ntcount = 0
                hsep (a.rhs map (elem => toRHS (elem, true))) <+> toAction (a)
            }

            def toAction (alt : Alternative) : Doc = {

                // The arguments are v1 .. vn. Normally these are passed straight through
                // to the constructor. However, if there is an annotation of the form 
                // n:m then the value passed through for argument n is the result of
                // passing the original value to the method m.
                val argList =
                    (1 to ntcount).map {
                        case n =>
                            val argName = "v" <> value (n)
                            (alt->transformer (n) match {
                                case Some (method) =>
                                    method <+> parens (argName)
                                case None =>
                                    argName
                             })
                    }

                // Pretty-printed argument list for constructor
                val args = hsep (argList, comma)

                if (alt->requiresNoAction)
                    empty
                else
                    braces (nest (line <>
                        "yyValue" <+> equal <+> 
                            (alt.action match {
                                case ApplyAction () =>
                                    "ParserSupport.apply (v2, v1)"
                                case DefaultAction () =>
                                    "new" <+> text (alt->constr) <+> parens (args)
                                case NoAction () =>
                                    // Not reachable
                                    empty
                                case TailAction (typeName, constr) =>
                                    toBraceSection ("new Action<" + typeName + "> ()",
                                        toBraceSection ("public " + typeName + " run (" + typeName + " left)",
                                            "return new" <+> constr <+> "(left, v1);"
                                        ) <> semi
                                    )
                             }) <> semi
                    ) <> line)

            }

            line <>
            (if (isConst) "constant" else "public") <+>
            text (astRule->typeName) <+> lhs.name <+> equal <>
            nest (
                line <>
                lsep2 (alts map toAlternative, "/") <> semi
            )

        }

        def toStringRule (stringRule : StringRule) : Doc = {

            val StringRule (lhs, alts) = stringRule

            def toAlternative (e : Element) : Doc =
                toRHS (e, false)

            line <>
            "public" <+> "String" <+> lhs.name <+> equal <>
            nest (
                line <>
                lsep2 (alts map toAlternative, "/") <> semi
            )

        }

        /**
         * Generate default implementations of various aspects, depending on the
         * flag settings. 
         */
        def toDefaults : Doc = {

            val layoutSpec = 
                if (flags.useDefaultLayout)
                    """
                    |// Default layout specification
                    |transient void Spacing   = (Space / Comment)*;
                    |transient void FSpacing  = (Space / Comment)+;
                    |transient void Space     = ' ' / '\t' / '\f' / EOL;
                    |transient void EOL       = '\r' '\n' / '\r' / '\n';
                    |transient void EOF       = !_;    
                    |""".stripMargin
                else
                    ""

            val commentSpec = 
                if (flags.useDefaultComments)
                    """
                    |// Default comment specification
                    |transient void Comment   = SLComment / MLComment;
                    |transient void SLComment = "//" (!EOL _)* EOL;
                    |transient void MLComment = "/*" (MLComment / !"*/" _)* "*/";
                    |""".stripMargin
                else
                    ""

            val wordsSpec =
                if (flags.useDefaultWords)
                    """
                    |// Default word specification
                    |String Identifier =
                    |    Word &{
                    |        ! contains (KEYWORDS, yyValue)
                    |    };
                    |
                    |String Word =
                    |    WordCharacters Spacing;
                    |
                    |transient String WordCharacters =
                    |    h:_ &{
                    |        Character.isJavaIdentifierStart (h)
                    |    } (t:_ &{
                    |        Character.isJavaIdentifierPart (t)
                    |    })*;
                    |""".stripMargin
                else
                    ""

            string (layoutSpec) <@>
            string (commentSpec) <@>
            string (wordsSpec)

        }

        // Initialise the tree so we can perform attribution on it
        initTree (grammar)

        // Convert grammar to a pretty printer document representing the translated
        // Rats! specification
        val code = pretty (toRats)

        // Put the code in the specified file
        write (genFile, code)

    }

}
