import sbt._

class TwtProject(info: ProjectInfo) extends ProcessorProject(info) {
  val configgy = "net.lag" % "configgy" % "1.6.3" intransitive()
  val dispatch = "net.databinder" %% "dispatch-twitter" % "0.7.8"
}
