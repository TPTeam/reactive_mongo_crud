package models.persistance

import reactivemongo.bson._
import play.api.Logger._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import MongoDBsConnector._
import models._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Promise

/** MongoDb collection operation wrapper  
 *
 *  @author  Andrea Peruffo <andrea.peruffo1982@gmail.com>
 */

trait PersistanceCompanion[T <: ModelObj] extends ReferenceJSONer[T] {
  
 
  lazy val dbName: String = defaultDb
  val collectionName: String
  
  //Context for the right database
  lazy val driver = dbs(dbName).driver
  lazy val connection = dbs(dbName).connection
  lazy val db = dbs(dbName).db
  
  lazy val collection = db(collectionName)
  
  implicit val reader: BSONDocumentReader[T]
  implicit val writer: BSONDocumentWriter[T]
 
  
  protected def _create(obj: T) = {
    val result = collection.save(obj)
    for (
        ret <- result
    ) yield {
      if (ret.ok) Some(obj)
      else  {
        logger.debug("Object not created")
        None
      }
    }
  }
 
  
  protected def _update(id: BSONObjectID,obj: T) = {
    logger.debug(s"DO update!") 
    val res = Promise[Option[T]]
    val result = collection.update(BSONDocument("_id" -> id), obj)
    
    result.onComplete
    {
      case Success(s) => {
        logger.debug(s"Object updated ${id.toString}") 
        logger.debug("OBJECT: "+obj.toString())
        res.trySuccess(Some(obj))
      }
      case Failure(f) => {
        logger.debug("Object not updated ${id.toString}")
        res.failure(f)
        None
      }
    }
    res.future
  }
 
  
  def _delete(id: BSONObjectID) = {
    val result = 
    		collection.remove(BSONDocument("_id" -> id))
    for (
        ret <- result
    ) yield
      if (ret.ok) true
      else {
        logger.debug(s"Object not deleted ${id.toString}")
        false
      }
  }
  
  def find(selector: BSONDocument, projection: BSONDocument) = {
	  collection.find(selector,projection)
  }

  def find(selector: BSONDocument) = {
    collection.find(selector)
  }
  
  def findOneById(id: BSONObjectID) = {
    collection.find(BSONDocument("_id" -> id)).one[T]
  }
  
  def findOneByIdString(id: String) = {
    collection.find(BSONDocument("_id" -> new BSONObjectID(id))).one[T]
  }
  
  def findAll = {
    collection.find(BSONDocument()).cursor[T]
  }
  
  object IdBSONReader extends BSONDocumentReader[BSONObjectID] {
    def read(doc: BSONDocument): BSONObjectID =
        doc.getAs[BSONObjectID]("_id").get
  }
  
  def findAllIds = {
    collection.find(BSONDocument(), BSONDocument("_id" -> 1)).cursor(IdBSONReader, defaultContext)
  }
  
  def count =
    db.command(reactivemongo.core.commands.Count(collectionName))

    
  def findByAttName(attName: String, attValue: String) = {
    (attName,attValue) match {
      case ("_id",_) => collection.find(BSONDocument("_id" -> new BSONObjectID(attValue))).cursor[T]
      case _ => collection.find(BSONDocument(attName -> attValue)).cursor[T]
    }
  }  
}