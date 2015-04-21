val myName = "reactive-mongo-plugin"
val myVersion = "0.1.26"
val myOrganization = "org.tpteam"

name := myName

version := myVersion

lazy val commonSettings = Seq(
  organization := myOrganization,
  version := myVersion,
  scalaVersion := "2.11.4"
)

lazy val root = (project in file("."))
    .enablePlugins(PlayScala)
    .settings(commonSettings: _*)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-remote" % "2.3.7",
  "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23",
  jdbc,
  anorm,
  cache,
  ws
)
