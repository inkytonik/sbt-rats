import org.kiama.util.Environments

object Analyser extends Environments {

    import ast._
    import org.kiama.==>
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.{chain, Chain, down}
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent

    def check (n : ASTNode) {
        n match {
            case d @ IdnDef (i) if d->entity == MultipleEntity =>
                message (d, i + " is declared more than once")

            // FIXME: Really should be the following, but second case never seems to be
            // reached if the first one is there...
            //
            // case u @ IdnUse (i) if u->entity == UnknownEntity =>
            //     message (u, i + " is not declared")

            // case HasParent (u @ IdnUse (i), _ : NonTerminal) =>
            //     println (i + " defined to be " + (u->entity))
            //     if (u->entity == Type ())
            //         message (u, "type " + i + " found where non-terminal expected")

            case u @ IdnUse (i) =>
                if (u->entity == UnknownEntity)
                    message (u, i + " is not declared")
                else if ((u.parent.isInstanceOf[NonTerminal]) && (u->entity == Type ()))
                    message (u, "type " + i + " found where non-terminal expected")

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
     * A non-terminal with the given type.
     */
    case class NonTerm (tipe : String) extends Entity

    /**
     * A type.
     */
    case class Type () extends Entity

    /**
     * The default environment, containing the grammar symbols that are 
     * either provided by the plugin or are expected to be defined by 
     * the user.
     */
    def defenv : Environment =
        rootenv ("Comment" -> NonTerm ("String"),
                 "Spacing" -> NonTerm ("void"),
                 "String" -> Type (),
                 "Word" -> NonTerm ("String"))

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
            case ASTRule (_, tipe, _, _) =>
                NonTerm (if (tipe == null) i else tipe.name)
            case StringRule (_, _) =>
                NonTerm ("String")
        }

    lazy val env=
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
                    case NonTerm (t) =>
                        t
                    case e =>
                        sys.error ("nttype: non-NonTerm " + e + " in NonTerm position")
                }
        }

    lazy val elemtype : Element => String =
        attr {
            case n : NonTerminal          => n->nttype
            case Opt (n : NonTerminal)    => "Option[%s]".format (n->nttype)
            case Rep (_, n : NonTerminal) => "List[%s]".format (n->nttype)
            case e =>
                sys.error ("elemtype: unexpected element kind " + e)
        }

}
