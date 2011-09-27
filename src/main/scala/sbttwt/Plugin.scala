package sbttwt

import sbt._
import Keys._

object Plugin extends sbt.Plugin {
  override lazy val settings = Seq(commands ++= Seq(TwtCommand.twt))
}
