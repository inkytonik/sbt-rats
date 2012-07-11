package ast

import org.kiama.attribution.Attributable
import org.kiama.output.Side
import scala.util.parsing.input.Positional

abstract class ASTNode extends Attributable with Positional

case class Grammar (pkg : List[String], header : String, rules : List[Rule]) extends ASTNode

sealed abstract class Rule extends ASTNode
case class ASTRule (idndef : IdnDef, tipe : IdnUse, alts : List[Alternative],
                    isConst : Boolean = false, anns: List[Annotation] = Nil) extends Rule
case class StringRule (idndef : IdnDef, alts : List[Element]) extends Rule
case class RatsSection (code : String) extends Rule

case class Alternative (rhs : List[Element], anns: List[Annotation],
                        action : Action) extends ASTNode

sealed abstract class Annotation extends ASTNode
case class Associativity (side : Side) extends Annotation
case class Constructor (name : String) extends Annotation
case class Transformation (num : Int, method : List[String], tipe : List[String]) extends Annotation
case class Parenthesized () extends Annotation
case class Precedence (level : Int) extends Annotation

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
case class NonTerminal (idnuse : IdnUse) extends Element
case class Not (e : Element) extends Element
case class Opt (e : Element) extends Element
case class Rep (zero : Boolean, e : Element) extends Element
case class Seqn (left : Element, right : Element) extends Element
case class StringLit (s : String) extends Element
case class Wildcard () extends Element

sealed abstract class Formatting extends Element
case class Nest (e : Element) extends Formatting
case class Newline () extends Formatting
case class Space () extends Formatting

sealed abstract class Identifier extends ASTNode {
    def name : String
}
case class IdnDef (name : String) extends Identifier
case class IdnUse (name : String) extends Identifier