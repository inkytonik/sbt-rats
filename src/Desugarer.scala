/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

/**
 * Desugarer from syntax definition to simplified form, parameterised by
 * the analyser to use.
 */
class Desugarer (analyser : Analyser) {

    import analyser.{actionTypeName, associativity, constr, idntype,
        isLeftRecursive, isTransferAlt, lhs, precedence, typeName}
    import ast._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.output.{LeftAssoc, NonAssoc, RightAssoc}
    import org.kiama.rewriting.Rewriter.{alltd, rewrite, rule}
    import scala.collection.mutable.ListBuffer

    /**
     * Desugar the directly left recursive rules.
     */
    def desugar (grammar : Grammar) : Grammar = {
        val grammar1 = fixTransferAlts (grammar)
        initTree (grammar1)
        removeLeftRecursion (grammar1)
    }

    /**
     * Detect transfer rules. Find rules that just transfer a semantic
     * value from RHS to LHS and mark them as requiring no action.
     */
    def fixTransferAlts (grammar : Grammar) = {

        val fixTransferAltsStrategy =
            alltd (
                rule {
                    case alt @ Alternative (rhs, anns, _) if alt->isTransferAlt =>
                        Alternative (rhs, anns, NoAction ())
                }
            )

        rewrite (fixTransferAltsStrategy) (grammar)

    }

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

        val removeLeftRecursionStrategy =
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
        val newg = rewrite (removeLeftRecursionStrategy) (grammar)

        grammar.copy (rules = newg.rules ++ newRules.result)

    }

    /**
     * Rewrite a single grammar rule to remove left recursion from it. Returns
     * the new version of the rule `r`, plus a list of new rules that are to 
     * be added to the grammar.
     */
    def removeLeftRecursiveAlternatives (astRule : ASTRule) : (ASTRule, Iterable[ASTRule]) = {

        // The type of the LHS non-terminal, is shared with new non-terminals
        val lhsnttype = (astRule.idndef)->idntype

        /**
         * Return a strategy that replaces identifier uses of one name by
         * which is generated and of the given type.
         */
        def replaceIdns (name1 : String, name2 : String, tipe : String) =
            alltd (
                rule {
                    case NTName (IdnUse (`name1`)) =>
                        NTGen (name2, tipe)
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
            def nt = NonTerminal (NTGen (ntname, lhsnttype))

            // Name for the non-terminal for the previous level
            val prevntname = "%s%d".format (astRule->lhs, prec - 1)

            // A non-terminal for the previous level
            def prevnt = NonTerminal (NTGen (prevntname, lhsnttype))

            // Name for the non-terminal for the tail
            val tailntname = ntname + "Tail"

            // A non-terminal for the tail
            def tailnt = NonTerminal (NTGen (tailntname, lhsnttype))

            // Collect the alternatives together by associativity
            val assocAlts = alts.groupBy (associativity)

            // Convenience short-hands to alternatives for each associativity
            val leftAssocAlts = assocAlts.getOrElse (LeftAssoc, Nil)
            val noneAssocAlts = assocAlts.getOrElse (NonAssoc, Nil)
            val rightAssocAlts = assocAlts.getOrElse (RightAssoc, Nil)

            // Buffer of rules that we will return
            val rules = new ListBuffer[ASTRule]

            // The base rule for the non-terminal at this precedence level needs to:
            //  - seed the iteration if there are any left associative alternatives
            //  - define the recursion for any right associative alternatives
            // This buffer accumulates the alternatives.
            val baseAlts = new ListBuffer[Alternative]

            // The iteration seed rule has the the form: nt = prevnt tailnt*
            // We only need it if there are left associative alternatives
            if (leftAssocAlts.nonEmpty)
                baseAlts.append (Alternative (List (prevnt, Rep (true, tailnt)),
                                              Nil,
                                              ApplyAction ()))

            // Strategy to replace old occurrences of the previous non-terminal
            // with the new generated one
            val renamer = replaceIdns (astRule.idndef.name, prevntname, lhsnttype)

            // Each right associative alternative turns into a single rule that defines
            // the recursive case to the next level
            val rightAlts =
                rightAssocAlts.map {
                    case Alternative (rhs, anns, _) =>
                        val newInitRHS = rewrite (renamer) (rhs.init)
                        Alternative (newInitRHS :+ nt, anns, DefaultAction ())
                }
            baseAlts.appendAll (rightAlts)

            // Each non-associative alternative turns into a single rule that defines
            // all recursive references to the next level
            val noneAlts =
                noneAssocAlts.map {
                    case Alternative (rhs, anns, _) =>
                        val newRHS = rewrite (renamer) (rhs)
                        Alternative (newRHS, anns, DefaultAction ())
                }
            baseAlts.appendAll (noneAlts)

            // If there are none/right recursive alternatives and no left recursive alternatives
            // we also need a fall-through alternative to get us to the next precedence level.
            // If there are left associative alternatives the seed rule takes care of this.
            if (leftAssocAlts.isEmpty && (noneAssocAlts.nonEmpty || rightAssocAlts.nonEmpty))
                baseAlts.append (Alternative (List (prevnt), Nil, NoAction ()))

            // Define the base rule using the base alternatives
            rules.append (ASTRule (IdnDef (ntname), IdnUse (astRule->typeName), baseAlts.result ()))

            // Generate the tail rule if there are any left associative alternatives
            if (leftAssocAlts.nonEmpty) {

                // Each left associative alternative turns into an alternative of a rule
                // that defines the tail of the iteration. We ignore any old action.
                val tailAlts =
                    leftAssocAlts.map {
                        case alt @ Alternative (rhs, _, _) =>
                            val newRHS = rewrite (renamer) (rhs.tail)
                            Alternative (newRHS, Nil, TailAction (astRule->typeName, alt->constr))
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
            val zerontidn = IdnDef ("%s0".format (astRule->lhs))

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
            val topnt = NonTerminal (NTGen ("%s%d".format (astRule->lhs, top),
                                            lhsnttype))

            // Each group gets translated together to give a list of new iterative rules.
            val precRules = recMap.flatMap (makeIterativeRules)

            // The head rule connects the original non-terminal to the precedence chain
            val headRule =
                ASTRule (astRule.idndef, IdnUse (astRule->typeName),
                         List (Alternative (List (topnt), null, NoAction ())))

            (newr, List (headRule) ++: precRules)

        }

    }

}
