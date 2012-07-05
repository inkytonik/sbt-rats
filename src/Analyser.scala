object Analyser {

    import ast._
    import org.kiama.==>
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.{chain, Chain}

    /**
     * Return a pair consisting of the set of keywords used in the grammar
     * and the set of non-keywords used. Keywords are defined to be literals
     * that containing only letters.
     */
    def partitionLiterals (g : Grammar) : (Set[String],Set[String]) =
        (g->literals).partition (_.forall (_.isLetter))

    lazy val literals : Chain[ASTNode,Set[String]] =
        chain (literalsin, literalsout)

    def literalsin (in : ASTNode => Set[String]) : ASTNode ==> Set[String] = {
        case _ : Grammar => Set ()
    }

    def literalsout (out : ASTNode => Set[String]) : ASTNode ==> Set[String] = {
        case n @ CharLit (s)   => n->(literals.in) + s
        case n @ StringLit (s) => n->(literals.in) + s
    }

}
