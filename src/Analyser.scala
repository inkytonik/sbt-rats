/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

import org.kiama.util.Environments

/**
 * Semantic analyser for syntax definitions, parameterised by the flags
 * that apply to this build.
 */
class Analyser (flags : Flags) extends Environments {

    import ast._
    import org.kiama.==>
    import org.kiama.attribution.Attribution.{attr, paramAttr}
    import org.kiama.attribution.Decorators.{chain, Chain, down}
    import org.kiama.output.{Fixity, Infix, LeftAssoc, NonAssoc, Postfix, Prefix,
        RightAssoc, Side}
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent

    def check (n : ASTNode) {
        n match {
            case d @ IdnDef (i) if d->entity == MultipleEntity =>
                message (d, i + " is declared more than once")

            case u @ IdnUse (i) =>
                if (u->entity == UnknownEntity)
                    message (u, i + " is not declared")
                else if ((u.parent.isInstanceOf[NonTerminal]) && (u->entity == Type ()))
                    message (u, "type " + i + " found where non-terminal expected")

            case b @ Block (_, n) =>
                b.parent match {
                    case alt : Alternative =>
                        val numalts = alt.rhs.length
                        if (b.index + 1 == n)
                            message (b, "block non-terminal can't refer to itself")
                        else if ((n < 1) || (n > numalts))
                            message (b, "block non-terminal reference " + n +
                                        " out of range 1.." + numalts)
                    case _ =>
                        // Do nothing
                }


            case _ =>
                // Do nothing by default
        }
        for (child <- n.children)
            check (child.asInstanceOf[ASTNode])
    }

    /**
     * Return a pair consisting of the set of keywords used in the grammar
     * and the set of non-keywords used. Keywords are defined to be literals
     * that containing only letters.
     */
    def partitionLiterals (g : Grammar) : (Set[String],Set[String]) =
        (g->literals).partition (_.forall (_.isLetter))

    // Entities

    /**
     * Non-terminals with a particular type
     */
    abstract class NonTerm extends Entity {
        def tipe : String
    }

    /**
     * A user-defined non-terminal with the given type and defined by the given
     * AST rule.
     */
    case class UserNonTerm (tipe : String, astRule : ASTRule) extends NonTerm

    /**
     * A user-defined non-terminal with the given type and defined by the given
     * Rats rule.
     */
    case class RatsNonTerm (tipe : String, ratsRule : RatsRule) extends NonTerm

    /**
     * A pre-defined non-terminal with the given type.
     */
    case class PreNonTerm (tipe : String) extends NonTerm

    /**
     * A type.
     */
    case class Type () extends Entity

    /**
     * The default environment, containing the grammar symbols that are 
     * either provided by the plugin or are expected to be defined by 
     * the user.
     */
    def defenv : Environment = {
        val possibleBindings =
            List (flags.useDefaultComments -> ("Comment" -> PreNonTerm ("void")),
                  flags.useDefaultLayout   -> ("EOF" -> PreNonTerm ("void")),
                  flags.useDefaultLayout   -> ("EOL" -> PreNonTerm ("void")),
                  flags.useDefaultWords    -> ("Identifier" -> PreNonTerm ("String")),
                  flags.useDefaultComments -> ("SLComment" -> PreNonTerm ("void")),
                  flags.useDefaultComments -> ("MLComment" -> PreNonTerm ("void")),
                  flags.useDefaultLayout   -> ("Space" -> PreNonTerm ("void")),
                  flags.useDefaultLayout   -> ("Spacing" -> PreNonTerm ("void")),
                  true                     -> ("String" -> Type ()),
                  true                     -> ("Void" -> Type ()),
                  flags.useDefaultWords    -> ("Word" -> PreNonTerm ("String")),
                  flags.useDefaultWords    -> ("WordCharacters" -> PreNonTerm ("String")))
        val bindings =
            possibleBindings.filter (_._1).map (_._2)
        rootenv (bindings : _*)
    }

    lazy val literals : Chain[ASTNode,Set[String]] =
        chain (literalsin, literalsout)

    def literalsin (in : ASTNode => Set[String]) : ASTNode ==> Set[String] = {
        case _ : Grammar => Set ()
    }

    def literalsout (out : ASTNode => Set[String]) : ASTNode ==> Set[String] = {
        case n @ CharLit (s)   => (n->out) + s
        case n @ StringLit (s) => (n->out) + s
    }

    // Name analysis

    lazy val preenv : Chain[ASTNode,Environment] =
        chain (envin, envout)

    def envin (in : ASTNode => Environment) : ASTNode ==> Environment = {
        case _ : Grammar => defenv
    }

    def envout (out : ASTNode => Environment) : ASTNode ==> Environment = {
        case n @ IdnDef (i) =>
            define (n->out, i,
                    if (isDefinedInScope (n->(preenv.in), i))
                        MultipleEntity
                    else
                        entityFromDecl (n, i))
    }

    def entityFromDecl (n : IdnDef, i : String) : Entity =
        n.parent match {
            case astRule @ ASTRule (_, tipe, _, _, _) =>
                UserNonTerm (if (tipe == null) i else tipe.name, astRule)
            case StringRule (_, _, save) =>
                if (save)
                    PreNonTerm ("String")
                else
                    PreNonTerm ("Void")
            case ratsRule @ RatsRule (_, tipe, _) =>
                RatsNonTerm (if (tipe == null) i else tipe.name, ratsRule)
        }

    lazy val env =
        down[ASTNode, Environment] {
            case n if n isRoot =>
                n->preenv
        }

    lazy val entity : Identifier => Entity =
        attr {
            case n =>
                lookup (n->env, n.name, UnknownEntity)
        }

    // Type analysis

    lazy val nttype : NonTerminal => String =
        attr {
            case NonTerminal (i) =>
                (i->entity) match {
                    case nt : NonTerm =>
                        nt.tipe
                    case e =>
                        sys.error ("nttype: non-NonTerm " + e + " for " + i +
                                   " in NonTerm position")
                }
        }

    lazy val elemtype : Element => String =
        attr {
            case n : NonTerminal =>
                n->nttype
            case Opt (n : NonTerminal) =>
                if (n->nttype == "Void")
                    "Void"
                else
                    "Option[%s]".format (n->nttype)
            case Rep (_, n : NonTerminal) =>
                if (n->nttype == "Void")
                    "Void"
                else
                    "List[%s]".format (n->nttype)
            case _ : Block =>
                "String"
            case e =>
                sys.error ("elemtype: unexpected element kind " + e)
        }

    // Patterns

    /**
     * Pattern to match a literal element.
     */
    object Literal {
        def unapply (e : Element) : Option[String] =
            e match {
                case CharLit (s)   => Some (s)
                case StringLit (s) => Some (s)
                case _             => None
            }
    }

    /**
     * Pattern to match a non-terminal name.
     */
    object NonTermIdn {
        def unapply (e : Element) : Option[String] =
            e match {
                case NonTerminal (IdnUse (name)) =>
                    Some (name)
                case _ =>
                    None
            }
    }

    // Properties

    /**
     * The name of the type that represents values of a particular rule.
     * Either given explicitly, or if implicit, the same as the LHS of 
     * the rule. Also works on 
     */
    lazy val typeName : ASTRule => String =
        attr {
            case astRule if astRule.tipe == null =>
                astRule.idndef.name
            case astRule =>
                astRule.tipe.name
        }

    /**
     * The action type of a recursive rule.
     */
    lazy val actionTypeName : ASTRule => String =
        attr {
            case astRule =>
                "Action<" + (astRule->typeName) + ">"
        }

    /**
     * Look for a particular annotation on a rule or, if it's a typed rule,
     * on the rule that defines its type.
     */
    def hasRuleAnnotation (astRule : ASTRule, ann : RuleAnnotation) : Boolean =
        if (astRule.tipe == null)
            (astRule.anns != null) && (astRule.anns contains (ann))
        else 
            (astRule.tipe)->entity match {
                case UserNonTerm (_, otherRule) =>
                    hasRuleAnnotation (otherRule, ann)
                case _ =>
                    false
            }

    /**
     * Is this rule to be prefixed by line breaks when pretty-printing?
     */
    lazy val isLinePP : ASTRule => Boolean =
        attr {
            case astRule =>
                hasRuleAnnotation (astRule, Line ())
        }

    /**
     * Is this rule to be parenthesized when pretty-printing?
     */
    lazy val isParenPP : ASTRule => Boolean =
        attr {
            case astRule =>
                hasRuleAnnotation (astRule, Parenthesized ())
        }

    /**
     * Is spacing turned on for this rule? I.e., does it not have a 
     * `nospacing` annotation?
     */
    lazy val hasSpacing : ASTRule => Boolean =
        attr {
            case astRule =>
                !hasRuleAnnotation (astRule, NoSpacing ())
        }

    /**
     * The precedence level of an alternative. If no explicit precedence level
     * is given in the annotations of the alternative, return one. If there is
     * more than one precedence annotation, the first one is used.
     */
    lazy val precedence : Alternative => Int =
        attr {
            case alt =>
                if (alt.anns == null)
                    1
                else
                    alt.anns.collect {
                        case Precedence (level) => level
                    } match {
                        case level :: _ => level
                        case _          => 1
                    }
        }

    /**
     * The LHS side symbol of a rule.
     */
    lazy val lhs : ASTRule => String =
        attr {
            case astRule =>
                astRule.idndef.name
        }

    /**
     * Whether an alternative is left-recursive: a sequence of more
     * than one element on the RHS and the first one is the LHS.
     */
    lazy val isLeftRecursive : Alternative => Boolean =
        attr {
            case alt =>
                alt.rhs.head == NonTerminal (IdnUse (alt->astrule->lhs))
        }

    /**
     * Whether an alternative is recursive: the LHS appears in the RHS
     * sequence.
     */
    lazy val isRecursive : Alternative => Boolean =
        attr {
            case alt =>
                alt.rhs contains NonTerminal (IdnUse (alt->astrule->lhs))
        }

    /**
     * The associativity of an alternative. If there are no associativity
     * annotations, it's left associative. Otherwise, it's the first such
     * annotation that applies.
     */
    lazy val associativity : Alternative => Side =
        attr {
            case alt =>
                if (alt.anns == null)
                    LeftAssoc
                else
                    alt.anns.collect {
                        case Associativity (side) =>
                            side
                    } match {
                        case assoc :: _ => assoc
                        case _          => LeftAssoc
                    }
        }

    /**
     * The ASTRule of an alternative.
     */
    lazy val astrule : Alternative => ASTRule =
        attr {
            case alt =>
                alt.parent.asInstanceOf[ASTRule]
        }

    /**
     * The optional constructor for an alternative. If there are constructor
     * annotations, take the first one. Otherwise, return None.
     */
    lazy val optConstr : Alternative => Option[String] =
        attr {
            case alt =>
                if (alt.anns == null)
                    None
                else
                    alt.anns.collect {
                        case Constructor (name) =>
                            name
                    } match {
                        case name :: _ => Some (name)
                        case _         => None
                    }
        }

    /**
     * The consructor for an alternative. If there are constructor
     * annotations, take the first one. Otherwise, use the left-hand 
     * side of the enclosing rule.
     */
    lazy val constr : Alternative => String =
        attr {
            case alt =>
                (alt->optConstr) match {
                    case None =>
                        alt->astrule->lhs
                    case Some (name) =>
                        name
                }
        }

    /**
     * Does the annotation list for this alternative contain an annotation
     * of the form n:m:_? If so, return Some (m), otherwise return None.
     * If there are multiple such annotations, the first one is used.
     */
    lazy val transformer : Int => Alternative => Option[String] =
        paramAttr {
            case n => {
                case alt =>
                    if (alt.anns == null)
                        None
                    else
                        alt.anns.collect {
                            case Transformation (m, method, _) if n == m =>
                                method
                        } match {
                            case method :: _ => Some (method.mkString ("."))
                            case _           => None
                        }
            }

        }

    /**
     * Whether or not an alternative requires an action. We don't need one
     * if the alternative has an explicit decoration that it requires no
     * action.
     */
    lazy val requiresNoAction : Alternative => Boolean =
        attr {
            case alt =>
                alt.action == NoAction ()
        }

    /**
     * Is this alternative a transfer alternative or not? A transfer
     * alternative is one that has a single non-terminal on the right-hand
     * side, the type of that non-terminal is the same as the type of the
     * rule and there are no annotations.
     */
    lazy val isTransferAlt : Alternative => Boolean =
        attr {
            case alt if alt.anns == null =>
                alt->syntaxElements match {
                    case List (nt : NonTerminal) =>
                        nt->nttype == alt->astrule->typeName
                    case _ =>
                        false
                }
            case _ =>
                false
        }

    /**
     * The elements of an alternative, ignoring the elements that don't
     * contribute to the abstract syntax.
     */
    lazy val syntaxElements : Alternative => List[Element] =
        attr {
            case alt =>
                alt.rhs.filterNot {
                    case _ : Formatting | _ : CharLit | _ : StringLit =>
                        true
                    case _ =>
                        false
                }
        }

    /**
     * Whether or not the alternative needs a pretty-printing clause:
     * if it has no action, if it's part of a parenthesized rule and
     * and features the recursive symbols, or if it's a transfer 
     * alternative. In the second case it will be handled by the paren
     * pretty printer.
     */
    lazy val requiresNoPPCase : Alternative => Boolean =
        attr {
            case alt =>
                (alt->requiresNoAction) ||
                ((alt->astrule->isParenPP) && (alt->isRecursive)) ||
                (alt->isTransferAlt)
        }

    /**
     * Alternatives of a rule that have something to say about the tree.
     */
    lazy val treeAlternatives : ASTRule => List[Alternative] =
        attr {
            case astRule => 
                astRule.alts.filter (alt => (alt.anns != null) && (alt.anns.length > 0))
        }

    /**
     * Map of field number to type as given by the alternative's
     * annotations.
     */
    lazy val fieldTypes : Alternative => Map[Int,String] =
        attr {
            case alt =>
                if (alt.anns == null)
                    Map.empty
                else
                    alt.anns.collect {
                        case Transformation (n, _, t) =>
                            (n, t.mkString ("."))
                    }.reverse.toMap
        }

    /**
     * List of Scala keywords used to avoid declaring a field whose name
     * is a keyword.
     */
    val scalaKeywords = List (
        "abstract", "case", "catch", "class", "def", "do", "else",
        "extends", "false", "final", "finally", "for", "forSome", "if",
        "implicit", "import", "lazy", "match", "new", "null", "object",
        "override", "package", "private", "protected", "return", "sealed",
        "super", "this", "throw", "trait", "true", "try", "type", "val",
        "var", "while", "with", "yield"
    )

    /**
     * Convert a non-terminal name to a field name. Prefix is a string that
     * we want to put on the beginning of the field name, suffix should go
     * on the end. Additonally, if the final result would clash with a
     * Scala keyword, add an addition "Field" suffix.
     */
    def nameToFieldName (prefix : String, name : String, suffix : String) : String = {
        val fieldName =
            if (prefix == "")
                "%s%s".format (name.toLowerCase, suffix)
            else
                "%s%s%s%s".format (prefix, name.head.toUpper, name.tail.toLowerCase,
                                   suffix)
        if (scalaKeywords contains fieldName)
            "%sField".format (fieldName)
        else
            fieldName
    }

    /**
     * Make a name for a field to represent a particular element.
     */
    lazy val fieldName : Element => String =
        attr {
            case NonTerminal (IdnUse (name)) =>
                nameToFieldName ("", name, "")
            case Opt (NonTerminal (IdnUse (name))) =>
                nameToFieldName ("opt", name, "")
            case Rep (zero, NonTerminal (IdnUse (name))) =>
                nameToFieldName (if (zero) "opt" else "", name, "s")
            case elem =>
                // Remove once ASTRule RHS elements are simplified so these are the only cases??
                sys.error ("fieldName: unexpected element kind " + elem)
        }

    /**
     * Collected information about an alternative: it's order (unary=1,
     * binary=2, operator, precedence, fixity and non-terminals. If we can't
     * tell or it's a case we don't support, return None.
     */
    lazy val orderOpPrecFixityNonterm : Alternative => Option[(Int, String, Int, Fixity, String, String)] =
        attr {
            case alt =>
                val lhsnt = alt->astrule->lhs
                alt.rhs match {

                    case List (elem @ NonTermIdn (nt), Literal (op)) if nt == lhsnt =>
                        Some ((1, op, alt->precedence, Postfix, elem->fieldName, ""))

                    case List (Literal (op), elem @ NonTermIdn (nt)) if nt == lhsnt =>
                        Some ((1, op, alt->precedence, Prefix, elem->fieldName, ""))

                    case List (elem1 @ NonTermIdn (nt1), Literal (op), elem2 @ NonTermIdn (nt2))
                            if (nt1 == lhsnt) && (nt2 == lhsnt) =>
                        val fixity = Infix (alt->associativity)
                        Some ((2, op, alt->precedence, fixity, elem1->fieldName, elem2->fieldName))

                    case elems =>
                        None

                }
        }

}
