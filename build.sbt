sbtPlugin := true

name := "sbt-twt"

organization := "com.eed3si9n"

version := "0.2.0"

description := "sbt plugin to tweet"

libraryDependencies ++= Seq("net.databinder" %% "dispatch-oauth" % "0.8.5",
  "net.databinder" %% "dispatch-core" % "0.8.5",
  "net.databinder" %% "dispatch-http-json" % "0.8.5",
  "net.databinder" %% "dispatch-lift-json" % "0.8.5",
  "net.databinder" %% "dispatch-oauth" % "0.8.5")
  
scalacOptions := Seq("-deprecation", "-unchecked")

publishTo <<= version { (v: String) =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if(v endsWith "-SNAPSHOT") Some("Scala Tools Nexus" at nexus + "snapshots/")
  else Some("Scala Tools Nexus" at nexus + "releases/")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishArtifact in (Compile, packageBin) := true

publishArtifact in (Test, packageBin) := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

publishMavenStyle := true

resolvers += "twttr.com Repo" at "http://maven.twttr.com"

seq(lsSettings :_*)

LsKeys.tags in LsKeys.lsync := Seq("sbt", "twitter")
