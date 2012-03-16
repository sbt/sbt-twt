sbtPlugin := true

name := "sbt-twt"

organization := "com.eed3si9n"

version := "0.2.1-SNAPSHOT"

description := "sbt plugin to tweet"

libraryDependencies ++= Seq("net.databinder" %% "dispatch-oauth" % "0.8.5",
  "net.databinder" %% "dispatch-core" % "0.8.5",
  "net.databinder" %% "dispatch-http-json" % "0.8.5",
  "net.databinder" %% "dispatch-lift-json" % "0.8.5",
  "net.databinder" %% "dispatch-oauth" % "0.8.5")
  
scalacOptions := Seq("-deprecation", "-unchecked")

publishArtifact in (Compile, packageBin) := true

publishArtifact in (Test, packageBin) := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

resolvers += "twttr.com Repo" at "http://maven.twttr.com"

// seq(lsSettings :_*)

// LsKeys.tags in LsKeys.lsync := Seq("sbt", "twitter")

publishMavenStyle := false

publishTo <<= (version) { version: String =>
   val scalasbt = "http://scalasbt.artifactoryonline.com/scalasbt/"
   val (name, u) = if (version.contains("-SNAPSHOT")) ("sbt-plugin-snapshots", scalasbt+"sbt-plugin-snapshots")
                   else ("sbt-plugin-releases", scalasbt+"sbt-plugin-releases")
   Some(Resolver.url(name, url(u))(Resolver.ivyStylePatterns))
}

credentials += Credentials(Path.userHome / ".ivy2" / ".sbtcredentials")
