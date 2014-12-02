package models.persistance

import reactivemongo.api._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.typesafe.config._
import akka.actor.ActorSystem

object MongoDBsConnector {
  
  import play.Play
  import play.Configuration
  lazy val configFile = Play.application().configuration()
  
  case class MongoDBConfig(driver: MongoDriver,connection: MongoConnection, db: DB)
  
  lazy val dbs: Map[String, MongoDBConfig] = cropInList(configFile.getConfigList("mongodbs")).map(c => {
	  val driver = new MongoDriver
	  val connection = driver.connection(List(c.getString("host")))
	  val name = c.getString("name")
	  val db = connection.db(name)
	  play.api.Logger.info(s"MongoDB db ${name} available")
	  (name -> MongoDBConfig(driver,connection,db))
    }).toMap
    
  lazy val defaultDb = dbs.head._1 
  
  def cropInList[T <: Configuration](in: java.util.List[T]): List[Configuration] = {
    def _cropIt(partial: List[Configuration]): List[Configuration] = {
      if (in.isEmpty()) partial
      else {
        val newPart = 
          (in.get(0)) match {
          	case x : Configuration =>
          		in.remove(0)
          		partial.::(x)
          	case _ =>
          	  in.remove(0)
          	  partial
          }
        _cropIt(newPart)
      }
    }
    _cropIt(List())
  }

}

object ReactiveMongoActorSystem {
  
    val config = ConfigFactory.load("reactive_actors.conf")
    
    val systemName = config.getString("actors-system")
  
    lazy val system = ActorSystem(systemName, config.getConfig(systemName))


}
