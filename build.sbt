name := """foods"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.4.2",
  "commons-io" % "commons-io" % "2.4"
)

