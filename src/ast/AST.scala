package ast

import org.kiama.attribution.Attributable
import scala.util.parsing.input.Positional

abstract class ASTNode extends Attributable with Positional

case class Grammar (pkg : List[String], header : String, rules : List[Rule]) extends ASTNode

sealed abstract class Rule extends ASTNode
case class ASTRule (idndef : IdnDef, tipe : IdnUse, alts : List[Alternative],
                    isConst : Boolean = false) extends Rule
case class StringRule (idndef : IdnDef, alts : List[Element]) extends Rule
case class RatsSection (code : String) extends Rule

case class Alternative (rhs : ASTElement, anns: List[Annotation],
                        action : Action) extends ASTNode

abstract class Annotation extends ASTNode
case class Constructor (name : String) extends Annotation
case class Transformation (num : Int, method : List[String], tipe : List[String]) extends Annotation
case class Associativity (isLeft : Boolean) extends Annotation
case class Precedence (level : Int) extends Annotation

sealed abstract class Action extends ASTNode
case class ApplyAction () extends Action
case class DefaultAction () extends Action
case class NoAction () extends Action
case class TailAction (tipe : String, constr : String) extends Action

sealed abstract class Element extends ASTNode
case class Alt (l : Element, r : Element) extends Element

sealed abstract class ASTElement extends Element
case class CharClass (s : String) extends ASTElement
case class CharLit (s : String) extends ASTElement
case class Epsilon () extends ASTElement
case class NonTerminal (idnuse : IdnUse) extends ASTElement
case class Not (e : ASTElement) extends ASTElement
case class Opt (e : ASTElement) extends ASTElement
case class Rep (zero : Boolean, e : ASTElement) extends ASTElement
case class Seqn (left : ASTElement, right : ASTElement) extends ASTElement
case class StringLit (s : String) extends ASTElement
case class Wildcard () extends ASTElement

abstract class Identifier extends ASTNode {
    def name : String
}
case class IdnDef (name : String) extends Identifier
case class IdnUse (name : String) extends Identifier