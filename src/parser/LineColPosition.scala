/*
 * This file is part of the sbt-rats plugin.
 * Copyright (c) 2012-2013 Anthony M Sloane, Macquarie University.
 * All rights reserved.
 * Distributed under the New BSD license.
 * See file LICENSE at top of distribution.
 */

package parser

import scala.util.parsing.input.{Position, Positional}

class LineColPosition (val line : Int, val column : Int) extends Position {

    override def < (that : Position) : Boolean =
        line < that.line ||
            line == that.line && column < that.column

    override def lineContents : String =
        throw new RuntimeException ("LineColPosition.lineContents not implemented")

    override def longString : String =
        throw new RuntimeException ("LineColPosition.longString not implemented")

    override def toString () : String =
        "" + line + "." + column

}
