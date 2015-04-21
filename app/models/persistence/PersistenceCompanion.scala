package models.persistence

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
import scala.concurrent.Future

/** MongoDb collection operation wrapper  
  *
  *  @author  Andrea Peruffo <andrea.peruffo1982@gmail.com>
  */

trait PersistenceCompanion[T <: ModelObj] extends ReferenceJSONer[T] {


  lazy val dbName: String = defaultDb
  val collectionName: String

  //Context for the right database
  lazy val driver = dbs(dbName).driver
  lazy val connection = dbs(dbName).connection
  lazy val db = dbs(dbName).db

  lazy val collection = db(collectionName)

  implicit val reader: BSONDocumentReader[T]
  implicit val writer: BSONDocumentWriter[T]


  protected def originalCreate(obj: T) = {
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


  protected def originalUpdate(id: BSONObjectID,obj: T) = {
    val res = Promise[Option[T]]
    val result = collection.update(BSONDocument("_id" -> id), obj)

    result.onComplete
    {
      case Success(s) => {
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


  protected def originalDelete(id: BSONObjectID) = {
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
    BSONObjectID.parse(id).toOption match {
      case Some(oid) =>
        collection.find(BSONDocument("_id" -> oid)).one[T]
      case _ =>
        Future.successful(None)
    }
  }

  def findAll = {
    collection.find(BSONDocument()).cursor[T]
  }

  object IdBSONReader extends BSONDocumentReader[BSONObjectID] {
    def read(doc: BSONDocument): BSONObjectID =
      doc.getAs[BSONObjectID]("_id").get
  }

  def idStringReader(stringField: String) = new BSONDocumentReader[(BSONObjectID,String)]{
    def read(doc: BSONDocument): (BSONObjectID,String) = {
      (doc.getAs[BSONObjectID]("_id").get,
        doc.getAs[String](stringField).get)
    }
  }

  def findAllIdAndField(str: String) = collection.find(BSONDocument(),BSONDocument("_id" -> 1, str -> 1)).cursor(idStringReader(str), defaultContext)

  def findOneIdAndField(id: BSONObjectID,str: String) = collection.find(BSONDocument("_id" -> id),BSONDocument("_id" -> 1, str -> 1)).cursor(idStringReader(str), defaultContext).headOption(defaultContext)

  def findAllIds = collection.find(BSONDocument(), BSONDocument("_id" -> 1)).cursor(IdBSONReader, defaultContext)

  def findAllIdsWithFilter(filter: BSONDocument) = collection.find(filter, BSONDocument("_id" -> 1)).cursor(IdBSONReader, defaultContext)

  def findOneIdById(id: BSONObjectID) = collection.find(BSONDocument("_id" -> id), BSONDocument("_id" -> 1)).cursor(IdBSONReader, defaultContext).headOption(defaultContext)

  def count = db.command(reactivemongo.core.commands.Count(collectionName))

  def findByAttName(attName: String, attValue: String): reactivemongo.api.Cursor[T] = {
    (attName,attValue) match {
      case ("_id",_) => {
        BSONObjectID.parse(attValue).toOption match {
          case Some(oid) =>
            collection.find(BSONDocument("_id" -> oid)).cursor[T]
          case _ =>
            collection.find(BSONDocument(attName -> attValue)).cursor[T]
        }
      }
      case _ => collection.find(BSONDocument(attName -> attValue)).cursor[T]
    }
  }

}