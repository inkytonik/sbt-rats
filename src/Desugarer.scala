object Desugarer {

    import Analyser.{actionTypeName, constr, first, init, isLeftAssociative,
        isLeftRecursive, lhs, precedence, tail, typeName}
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

        val newRules = new ListBuffer[ASTRule]

        val removeLeftRecursiveRules =
            alltd (
                rule {
                    case r : ASTRule =>
                        // Rewrite r and save the new rules for adding in later
                        val (newr, rules) = removeLeftRecursiveAlternatives (r)
                        newRules.appendAll (rules)
                        newr
                }
            )

        // Rewrite to get a transformed grammar and some new rules to add to it
        val newg = rewrite (removeLeftRecursiveRules) (grammar)

        Grammar (grammar.pkg, grammar.header, newg.rules ++ newRules.result)

    }

    /**
     * Rewrite a single grammar rule to remove left recursion from it. Returns
     * the new version of the rule `r`, plus a list of new rules that are to 
     * be added to the grammar.
     */
    def removeLeftRecursiveAlternatives (astRule : ASTRule) : (ASTRule, Iterable[ASTRule]) = {

        /**
         * Return a strategy that replaces identifier uses of one name by another.
         */
        def replaceIdns (name1 : String, name2 : String) =
            alltd (
                rule {
                    case IdnUse (name1) =>
                        IdnUse (name2)
                }
            )

        /**
         * Make iterative rules from a set of alternatives at the same precedence
         * level.
         */
        def makeIterativeRules (precalt : (Int, List[Alternative])) : List[ASTRule] = {

            val prec = precalt._1
            val alts = precalt._2

            // Name of for a non-terminal at this level
            val ntname = "%s%d".format (astRule->lhs, prec)

            // A non-terminal for this level
            def nt = NonTerminal (IdnUse (ntname))

            // Name for the non-terminal for the previous level
            val prevntname = "%s%d".format (astRule->lhs, prec - 1)

            // A non-terminal for the previous level
            def prevnt = NonTerminal (IdnUse (prevntname))

            // Name for the non-terminal for the tail
            val tailntname = ntname + "Tail"

            // A non-terminal for the tail
            def tailnt = NonTerminal (IdnUse (tailntname))

            // Partition alternative into left and right associative ones
            val (leftAssocAlts, rightAssocAlts) = alts.partition (isLeftAssociative)

            // Buffer of rules that we will return
            val rules = new ListBuffer[ASTRule]

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
                        val newInitRHS = rewrite (replaceIdns (astRule.idndef.name, prevntname)) (rhs->init)
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
            rules.append (ASTRule (IdnDef (ntname), IdnUse (astRule->typeName), baseAlts.result ()))

            // Generate the tail rule if there are any left associative alternatives
            if (! leftAssocAlts.isEmpty) {

                // Each left associative alternative turns into an alternative of a rule
                // that defines the tail of the iteration. We ignore any old action.
                val tailAlts =
                    leftAssocAlts.map {
                        case alt @ Alternative (rhs : Seqn, _, _) =>
                            val newRHS = rewrite (replaceIdns (astRule.idndef.name, prevntname)) (rhs->tail)
                            Alternative (newRHS, Nil, TailAction (astRule->typeName, alt->constr))
                        case _ =>
                            sys.error ("non-Seqn tail alternative leftAssocAlt found in makeIterativeRules")
                    }

                // Define the tail rule
                rules.append (ASTRule (IdnDef (tailntname), IdnUse (astRule->actionTypeName),
                              tailAlts, true))

            }

            rules.result ()

        }

        // Partition the alternatives into recursive ones and non-recursive ones
        val (recursiveAlts, nonRecursiveAlts) = astRule.alts.partition (isLeftRecursive)

        // If there are no recursive alternatives, don't change the rule
        if (recursiveAlts.isEmpty) {

            (astRule, Nil)

        } else {

            // The zero-level LHS for this iteration
            def zerontidn = IdnDef ("%s0".format (astRule->lhs))

            /**
             * The new rule that replaces the non-recursive alternatives of the old rule.
             * It defines the base level non-terminal and the type is given by the type 
             * of the old non-terminal.
             */
            val newr = ASTRule (zerontidn, IdnUse (astRule->typeName), nonRecursiveAlts)

            // Group the recursive alternatives by precedence level
            val recMap = recursiveAlts.groupBy (precedence)

            // Calculate top precedence level
            // FIXME: avoid this extra pass over recursive alternatives
            val top = recursiveAlts.foldLeft (0) {
                          case (p, alt) => p.max (precedence (alt))
                      }

            // The "topmost" (i.e. lowest precedence) non-terminal for this iteration
            def topnt = NonTerminal (IdnUse ("%s%d".format (astRule->lhs, top)))

            // Each group gets translated together to give a list of new iterative rules.
            val precRules = recMap.flatMap (makeIterativeRules)

            // The head rule connects the original non-terminal to the precedence chain
            val headRule =
                ASTRule (astRule.idndef, IdnUse (astRule->typeName),
                         List (Alternative (topnt, null, NoAction ())))

            (newr, List (headRule) ++: precRules)

        }

    }

}
