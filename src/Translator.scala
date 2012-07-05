import org.kiama.output.PrettyPrinter

object Translator extends PrettyPrinter {
    
    import ast._
    import Analyser.partitionLiterals
    import org.kiama.attribution.Attribution.initTree

    def translate (flags : Flags, grammar : Grammar) : String = {

        def toRats (grammar : Grammar) : Doc = {
            val (keywords, symbols) = partitionLiterals (grammar)
            "module" <+> grammar.pkg.mkString (".") <> semi <@>
            toHeader (grammar.header, keywords) <@>
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
            ) <@>
            line <>
            "option setOfString (KEYWORDS);"

        def toBraceSection (keyword : String, contents : Doc, prefix : Doc = empty) : Doc =
            keyword <+> braces (prefix <> nest (line <> contents) <> line)

        def toKeywords (keywords : Set[String]) : Doc =
            "add" <+> parens (
                toBraceSection ("KEYWORDS, new String[]",
                    fillsep (keywords.toSeq.sorted map (s => dquotes (text (s))), comma)
                )
            ) <> semi

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
                case g : GrammarRule   => toGrammarRule (g)
                case RatsSection (c)   => string (c.mkString)
                case CommentDef (elem) => empty // FIXME
            }

        def toGrammarRule (grammarRule : GrammarRule) : Doc = {

            val GrammarRule (lhs, declType, alts, isConst) = grammarRule

            val tipe = if (declType == null) lhs.s else declType

            var ntcount = 0

            def toAlternative (a : Alternative) : Doc = {
                ntcount = 0
                toRHS (a.rhs, true) <+> toAction (a)
            }

            def toLiteral (s : String) : Doc = {
                val prefix : Doc = if (tipe == "String") "" else ("void" <> colon)
                val form = if (s.forall (_.isLetter)) "Word" else "Symbol"
                val suffix = if (tipe == "String") empty else colon <> form
                prefix <> dquotes (s) <> suffix
            }

            def bind (doc : Doc, cond : Boolean) : Doc =
                if (cond) {
                    ntcount = ntcount + 1
                    "v" <> value (ntcount) <> colon <> doc
                } else
                    doc

            def toRHS (elem : Element, addBindings : Boolean = false) : Doc =
                elem match {

                    case NonTerminal (str)  => bind (str, (tipe != "String") && addBindings)

                    case Not (elem)         => "!" <> parens (toRHS (elem))
                    case Opt (elem)         => bind (parens (toRHS (elem)) <> "?",
                                                     addBindings)
                    case Rep (zero, elem)   => bind (parens (toRHS (elem)) <> (if (zero) "*" else "+"),
                                                     addBindings)

                    case Alt (left, right)  => parens (toRHS (left) <> "/" <> toRHS (right))
                    case Seqn (left, right) => toRHS (left, addBindings) <+> toRHS (right, addBindings)

                    case CharLit (str)      => toLiteral (str)
                    case StringLit (str)    => toLiteral (str)

                    case CharClass (str)    => brackets (str)

                    case Epsilon ()         => "/* empty */"
                    case Wildcard ()        => "_"

                }

            def toAction (alt : Alternative) : Doc = {

                val constr =
                    alt.anns match {
                        case con :: _ => con
                        case _        => tipe
                    }

                /**
                 * Does the annotation list for this alternative contain an annotation
                 * of the form n:m? If so, return Some (m), otherwise return None.
                 * If there are multiple such annotations, the first one is used.
                 */
                def hasNAnnotation (n : Int) : Option[String] =
                    if (alt.anns == null)
                        None
                    else {
                        val nString = n.toString + ":"
                        alt.anns.foldLeft (None : Option[String]) {
                            case (o, ann) =>
                                if (ann.startsWith (nString))
                                    Some (ann.drop (nString.length))
                                else
                                    o
                        }
                    }

                // The arguments are v1 .. vn. Normally these are passed straight through
                // to the constructor. However, if there is an annotation of the form 
                // n:m then the value passed through for argument n is the result of
                // passing the original value to the method m.
                val argList =
                    (1 to ntcount).map {
                        case n =>
                            val argName = "v" <> value (n)
                            (hasNAnnotation (n) match {
                                case Some (method) =>
                                    method <+> parens (argName)
                                case None =>
                                    argName
                             })
                    }

                // Pretty-printed argument list for constructor
                val args = hsep (argList, comma)

                // No action at all if a) the type is String, b) the alternative is tagged as
                // requiring no action, or c) it's a transfer rule among other rules.
                if ((tipe == "String") || (alt.action == NoAction ()) ||
                        ((alt.anns == null) && (alts.length > 1)))
                    empty
                else
                    braces (nest (line <>
                        "yyValue" <+> equal <+> 
                            (alt.action match {
                                case ApplyAction () =>
                                    "ParserSupport.apply (v2, v1)"
                                case DefaultAction () =>
                                    "new" <+> constr <+> parens (args)
                                case NoAction () =>
                                    // Not reachable
                                    empty
                                case TailAction (tipe, constr) =>
                                    toBraceSection ("new Action<" + tipe + "> ()",
                                        toBraceSection ("public " + tipe + " run (" + tipe + " left)",
                                            "return new" <+> constr <+> "(left, v1);"
                                        ) <> semi
                                    )
                             }) <> semi
                    ) <> line)

            }

            line <>
            (if (isConst) "constant " else empty) <>
            "public" <+> tipe <+> lhs.s <+> equal <>
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
                    |transient void SLComment = "//" (![\n\r] _)* EOL;
                    |transient void MLComment = "/*" ('*' !'/' / !'*' _)* "*/";
                    |""".stripMargin
                else
                    ""

            val wordsSpec =
                if (flags.useDefaultWords)
                    """
                    |// Default word specification
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
        pretty (toRats (grammar))

    }

}
