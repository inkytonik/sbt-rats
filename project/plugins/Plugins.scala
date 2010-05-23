/*
 * This file is part of sbt-rats.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
 */

import sbt._

class Plugins (info : ProjectInfo) extends PluginDefinition (info) {
    val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"
    val posterous = "net.databinder" % "posterous-sbt" % "0.1.4"
}
