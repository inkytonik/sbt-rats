package ast

import org.kiama.attribution.Attributable

abstract class ASTNode extends Attributable

case class Grammar (pkg : List[String], header : String, rules : List[Rule]) extends ASTNode

sealed abstract class Rule extends ASTNode

case class GrammarRule (lhs : NonTerminal, tipe : String, alts : List[Alternative],
                        isConst : Boolean = false) extends Rule

case class Alternative (rhs : Element, anns: List[String], action : Action) extends ASTNode

sealed abstract class Action extends ASTNode
case class ApplyAction () extends Action
case class DefaultAction () extends Action
case class NoAction () extends Action
case class TailAction (tipe : String, constr : String) extends Action

sealed abstract class Element extends ASTNode
case class Alt (l : Element, r : Element) extends Element
case class CharClass (s : String) extends Element
case class CharLit (s : String) extends Element
case class Epsilon () extends Element
case class NonTerminal (s : String) extends Element
case class Not (e : Element) extends Element
case class Opt (e : Element) extends Element
case class Rep (zero : Boolean, e : Element) extends Element
case class Seqn (left : Element, right : Element) extends Element
case class StringLit (s : String) extends Element
case class Wildcard () extends Element

case class CommentDef (elem : Element) extends Rule

case class RatsSection (code : String) extends Rule
