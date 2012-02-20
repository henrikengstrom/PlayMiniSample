import sbt._
import Keys._

object Build extends Build {
  lazy val root = Project(id = "playminisample", 
  	base = file("."), settings = Project.defaultSettings).settings(
    resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
    resolvers += "Typesafe Snapshot Repo" at "http://repo.typesafe.com/typesafe/snapshots/",
    libraryDependencies += "com.typesafe" %% "play-mini" % "2.0-RC3-SNAPSHOT",
    mainClass in (Compile, run) := Some("play.core.server.NettyServer"))
}
