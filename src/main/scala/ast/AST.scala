/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012-2020 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

package ast

import org.kiama.attribution.Attributable
import org.kiama.output.Side

abstract class ASTNode extends Attributable

case class Grammar (module : List[String], header : String, astHeader : String,
                    body : String, options : List[SyntaxOption],
                    rules : List[Rule]) extends ASTNode

sealed abstract class SyntaxOption extends ASTNode
case class Indentation (n : Int) extends SyntaxOption
case class RelativeFilenames () extends SyntaxOption
case class SetOfString (name : String) extends SyntaxOption
case class Verbose () extends SyntaxOption
case class Width (n : Int) extends SyntaxOption

sealed abstract class Rule extends ASTNode
case class ASTRule (idndef : IdnDef, typeIdn : IdnUse, alts : List[Alternative],
                    isConst : Boolean = false, anns: List[RuleAnnotation] = Nil) extends Rule
case class StringRule (idndef : IdnDef, typeIdn : IdnUse, alts : List[Element]) extends Rule

sealed abstract class RatsSection extends Rule
case class RatsBlock (code : String) extends RatsSection
case class RatsRule (idndef : IdnDef, typeIdn : IdnUse, code : String) extends RatsSection

case class Alternative (rhs : List[Element], anns: List[AltAnnotation],
                        action : Action) extends ASTNode

sealed abstract class RuleAnnotation extends ASTNode
case class Line () extends RuleAnnotation
case class Nested () extends RuleAnnotation
case class NoSpacing () extends RuleAnnotation
case class Parenthesized () extends RuleAnnotation

sealed abstract class AltAnnotation extends ASTNode
case class Associativity (side : Side) extends AltAnnotation
case class Constructor (idndef : IdnDef) extends AltAnnotation
case class Precedence (level : Int) extends AltAnnotation
case class Transformation (num : Int, method : List[String], typeNames : List[String]) extends AltAnnotation

sealed abstract class Type extends ASTNode
case class AltType (lt : Type, rt : Type) extends Type
case class ConsType () extends Type
case class NamedType (s : String) extends Type
case class OptionType (t : Type) extends Type
case class RepType (t : Type) extends Type
case class SeqnType (lt : Type, rt : Type) extends Type
case class TypeType () extends Type
case class UnknownType () extends Type

sealed abstract class Action extends ASTNode
case class ApplyAction () extends Action
case class ConsAction (tipe : Type) extends Action
case class DefaultAction () extends Action
case class NilAction () extends Action
case class NoAction () extends Action
case class SingletonAction (typeName : String) extends Action
case class TailAction (typeName : String, constr : String) extends Action

sealed abstract class Element extends ASTNode
case class Alt (l : Element, r : Element) extends Element
case class And (e : Element) extends Element
case class Block (name : String, n : Int) extends Element
case class CharClass (s : String) extends Element
case class CharLit (lit : Literal) extends Element
case class Epsilon () extends Element
case class NonTerminal (ntuse : NTUse) extends Element
case class Not (e : Element) extends Element
case class Opt (e : Element) extends Element
case class Rep (zero : Boolean, e : Element, sep : Element) extends Element
case class Seqn (left : Element, right : Element) extends Element
case class StringLit (lit : Literal) extends Element
case class Wildcard () extends Element

/**
* A literal from the specification, parsed into its constituent character
* specifications.
*/
case class Literal (ss : List[String]) {

     /**
      * The string made up of the literal's character specifications.
      */
     val s = ss.mkString

     /**
      * A string made up of the literal's pieces translated so that Scala can handle
      * them as the content of a double-quoted string literal.
      *
      * Translations applied:
      *   - \-, \[, \] not supported by Scala (translated to -, [, ])
      *   - \' not needed in double quoted string (translated to ')
      *   - octal literals \o \oo \ooo are deprecated (translate to unicode escape)
      */
     val escaped : String = {
         val output = new StringBuilder
         val OctalEscape = """\\([0-3][0-7][0-7]|[0-7][0-7]|[0-7])""".r
         for (s <- ss) {
             s match {
                 case "\"" =>
                     output.append ("\\\"")
                 case "\\-" =>
                     output.append ("-")
                 case "\\[" =>
                     output.append ("[")
                 case "\\]" =>
                     output.append ("]")
                 case "\\'" =>
                     output.append ("'")
                 case OctalEscape (digits) =>
                     val n = Integer.parseInt (digits, 8)
                     output.append (s"\\u${"%04x".format(n)}")
                 case _ =>
                     output.append (s)
             }
         }
         output.result ()
     }

}

sealed abstract class NTUse extends ASTNode
case class NTName (idnuse : IdnUse) extends NTUse
case class NTGen (name : String, tipe : Type) extends NTUse

sealed abstract class Formatting extends Element
case class Nest (e : Element, newline : Boolean) extends Formatting
case class Newline () extends Formatting
case class Space () extends Formatting

sealed abstract class Identifier extends ASTNode {
    def name : String
}
case class IdnDef (name : String) extends Identifier
case class IdnUse (name : String) extends Identifier
