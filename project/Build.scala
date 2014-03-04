import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "reactive_mongo_plugin"
  val appVersion      = "0.0.12"

  val appDependencies = Seq(
    "org.reactivemongo" %% "reactivemongo" % "0.10.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    scalacOptions ++= Seq("-feature","-language:reflectiveCalls")
  )


}