object Desugarer {

    import ast._
    import org.kiama.rewriting.Rewriter.{alltd, rewrite, rule}
    import scala.collection.mutable.ListBuffer

    /**
     * Desugar the directly left recursive rules.
     */
    def desugar (grammar : Grammar) : Grammar =
        removeLeftRecursion (grammar)

    /**
     * Remove direct left recursive rules and replace with equivalent iterative
     * rules with supplementary action-based rules to construct the correct
     * semantic value.
     *
     * Example to illustrate the transformation that is performed:
     * 
     * Exp Exp =
     *         Exp Op1 Exp   {C1, left, 1}
     *      /  Exp Op2 Exp   {C2, left, 2}
     *      /  Foo           {C3}.
     * 
     * becomes
     *     
     * Exp Exp0 =
     *     Foo {C3}.
     * 
     * Exp Exp1 ::= seed:Exp0 actions:Exp1Tail* {
     *     yyValue = apply (actions, seed)
     * };
     * 
     * constant Action<Exp> Exp1Tail =
     *     Op1 right:Exp {
     *         yyValue = new Action<Exp> () {
     *             public Exp run (Exp left) {
     *                 return new C1 (left, right);
     *             }
     *         };
     *     };
     * 
     * Exp Exp2 ::= seed:Exp1 actions:Exp2Tail* {
     *     yyValue = apply (actions, seed)
     * };
     * 
     * constant Action<Exp> Exp2Tail =
     *     Op2 right:Exp1 {
     *         yyValue = new Action<Exp> () {
     *             public Exp run (Exp left) {
     *                 return new C2 (left, right);
     *             }
     *         };
     *     };
     *
     * The actions for the base rules and the tails are encoded as follows:
     *
     * ApplyAction == {
     *     yyValue = apply (actions, seed)
     * };
     *
     * TailAction (t, c) ==  {
     *     yyValue = new Action<Exp> () {
     *         public t run (t left) {
     *             return new c (left, right);
     *         }
     *     };
     * };
     */
    def removeLeftRecursion (grammar : Grammar) = {

        val newRules = new ListBuffer[GrammarRule]

        val removalStrategy =
            alltd (
                rule {
                    case r : GrammarRule =>
                        // Rewrite r and save the new rules for adding in later
                        val (newr, rules) = removeLeftRecursiveAlternatives (r)
                        newRules.appendAll (rules)
                        newr
                }
            )

        // Rewrite to get a transformed grammar and some new rules to add to it
        val newg = rewrite (removalStrategy) (grammar)

        Grammar (grammar.pkg, grammar.header, newg.rules ++ newRules.result)

    }

    /**
     * Rewrite a single grammar rule to remove left recursion from it. Returns
     * the new version of the rule `r`, plus a list of new rules that are to 
     * be added to the grammar.
     */
    def removeLeftRecursiveAlternatives (r : GrammarRule) : (GrammarRule, Iterable[GrammarRule]) = {

        /**
         * The first element in a sequence.
         */
        def firstElement (elem : Element) : Element =
            elem match {
                case Seqn (l, _) => firstElement (l)
                case _           => elem
            }

        /**
         * The tail elements in a sequence.
         */
        def tailElements (seqn : Seqn) : Element =
            seqn match {
                case Seqn (l : Seqn, r) =>
                    Seqn (tailElements (l), r)
                case Seqn (l, r) =>
                    r
            }

        /**
         * The initial elements in a sequence. I.e., all but the last one.
         */
        def initElements (seqn : Seqn) : Element =
            seqn.left

        /**
         * Return a strategy that replaces a non-terminal `nt1` with another `nt2`
         * wherever it appears in an element.
         */
        def replaceNonTerminals (nt1 : NonTerminal, nt2 : NonTerminal) =
            alltd (
                rule {
                    case `nt1` => nt2.clone
                }
            )

        /**
         * Return whether an alternative is left-recursive: a sequence of more
         * than one element on the RHS and the first one is the LHS.
         * FIXME: will do funny things if the RHS is just the LHS. Should have
         * semantic error for this case.
         */
        def isLeftRecursive (alt : Alternative) : Boolean =
            firstElement (alt.rhs) == r.lhs

        // Type of this rule (FIXME duplicated from Translator.scala)
        val tipe = if (r.tipe == null) r.lhs.s else r.tipe

        /**
         * Return the precedence level of a recursive alternative. If no explicit
         * precedence level is given in the annotations of the alternative, return
         * level one. If there is more than one numeric annotation, the first one
         * is returned.
         */
        def precedence (alt : Alternative) : Int = 
            alt.anns.filter (_.forall (_.isDigit)) match {
                case Nil => 1
                case l   => l.head.toInt
            }

        /**
         * Make iterative rules from a set of alternatives at the same precedence
         * level.
         */
        def makeIterativeRules (precalt : (Int, List[Alternative])) : List[GrammarRule] = {

            val prec = precalt._1
            val alts = precalt._2

            // A non-terminal for this level
            def nt = NonTerminal (r.lhs.s + prec.toString)

            // A non-terminal for the previous level
            def prevnt = NonTerminal (r.lhs.s + (prec - 1).toString)

            // A non-terminal for the tail
            def tailnt = NonTerminal (nt.s + "Tail")

            /**
             * Return whether the alternative is right associative or not. If there
             * is no `right` annotation, we assume it's left associative.
             */
            def isRightAssociative (alt : Alternative) : Boolean =
                alt.anns.contains ("right")

            // FIXME: duplicated from Translator.toAction
            def constr (alt : Alternative) : String =
                alt.anns match {
                    case con :: _ => con
                    case _        => tipe
                }

            // Partition alternative into left and right associative ones
            val (rightAssocAlts, leftAssocAlts) = alts.partition (isRightAssociative)

            // Buffer of rules that we will return
            val rules = new ListBuffer[GrammarRule]

            // The base rule for the non-terminal at this precedence level needs to:
            //  - seed the iteration if there are any left associative alternatives
            //  - define the recursion for any right associative alternatives
            // This buffer accumulates the alternatives.
            val baseAlts = new ListBuffer[Alternative]

            // The iteration seed rule has the the form: nt = prevnt tailnt*
            // We only need it if there are left associative alternatives
            if (! leftAssocAlts.isEmpty)
                baseAlts.append (Alternative (Seqn (prevnt, Rep (true, tailnt)),
                                              Nil,
                                              ApplyAction ()))

            // Each right associative alternative turns into a single rule that defines
            // the recursive case to the next level
            val recurseAlts =
                rightAssocAlts.map {
                    case Alternative (rhs : Seqn, anns, _) =>
                        val initRHS = initElements (rhs)
                        val newInitRHS = rewrite (replaceNonTerminals (r.lhs, prevnt)) (initRHS)
                        val newRHS = Seqn (newInitRHS, nt)
                        Alternative (newRHS, anns, DefaultAction ())
                    case _ =>
                        sys.error ("non-Seqn tail alternative rightAssocAlt found in makeIterativeRules")
                }
            baseAlts.appendAll (recurseAlts)

            // If there are right recursive alternatives and no left recursive alternatives
            // we also need a fall-through alternative to get us to the next precedence level.
            // If there are left associative alternatives the seed rule takes care of this.
            if (leftAssocAlts.isEmpty && (! rightAssocAlts.isEmpty))
                baseAlts.append (Alternative (prevnt, Nil, NoAction ()))

            // Define the base rule using the base alternatives
            rules.append (GrammarRule (nt, tipe, baseAlts.result ()))

            // Generate the tail rule if there are any left associative alternatives
            if (! leftAssocAlts.isEmpty) {

                // Each left associative alternative turns into an alternative of a rule
                // that defines the tail of the iteration. We ignore any old action.
                val tailAlts =
                    leftAssocAlts.map {
                        case alt @ Alternative (rhs : Seqn, _, _) =>
                            val tailRHS = tailElements (rhs)
                            val newRHS = rewrite (replaceNonTerminals (r.lhs, prevnt)) (tailRHS)
                            Alternative (newRHS, Nil, TailAction (tipe, constr (alt)))
                        case _ =>
                            sys.error ("non-Seqn tail alternative leftAssocAlt found in makeIterativeRules")
                    }

                // The type of actions for this rule
                val actionType = "Action<" + tipe + ">"

                // Define the tail rule
                rules.append (GrammarRule (tailnt, actionType, tailAlts, true))

            }

            rules.result ()

        }

        // Partition the alternatives into recursive ones and non-recursive ones
        val (recursiveAlts, nonRecursiveAlts) = r.alts.partition (isLeftRecursive)

        // If there are no recursive alternatives, don't change the rule
        if (recursiveAlts.isEmpty) {

            (r, Nil)

        } else {

            /**
             * The new rule that replaces the non-recursive alternatives of the old rule.
             * It defines the base level non-terminal and the type is given by the tipe 
             * of the old non-terminal.
             */
            val newr = GrammarRule (NonTerminal (r.lhs.s + "0"), tipe, nonRecursiveAlts)

            // Group the recursive alternatives by precedence level
            val recMap = recursiveAlts.groupBy (precedence)

            // Calculate top precedence level
            // FIXME: avoid this extra pass over recursive alternatives
            val top = recursiveAlts.foldLeft (0) {
                          case (p, alt) => p.max (precedence (alt))
                      }

            // Each group gets translated together to give a list of new iterative rules
            // Top is the topmost precedence level used.
            val precRules = recMap.flatMap (makeIterativeRules)

            // The head rule connects the original non-terminal to the precedence chain
            val headRule =
                GrammarRule (r.lhs, tipe,
                    List (Alternative (NonTerminal (r.lhs.s + top.toString), null,
                                       NoAction ())))

            (newr, List (headRule) ++: precRules)

        }

    }

}
