import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "reactive_mongo_plugin"
  val appVersion      = "0.0.16"

  val appDependencies = Seq(
    "com.typesafe.akka" %% "akka-remote" % "2.2.3",  
    "org.reactivemongo" %% "reactivemongo" % "0.10.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalacOptions ++= Seq("-feature","-language:reflectiveCalls"),
    resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
  )


}

