package models

import models.persistance._
import reactivemongo.bson._
import scala.concurrent._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.language.implicitConversions
import scala.util.Success
import scala.util.Failure
import models.persistance.PersistanceCompanion
import play.api.Logger._

case class Reference[+T <: ModelObj](id: BSONObjectID)(implicit resolver: PersistanceCompanion[T]) {
  
  def resolve =
    resolver.findOneById(id)
    
  def mySingleton: PersistanceCompanion[_] = resolver
    
}

trait ReferenceJSONer[A <: ModelObj] {
  self: PersistanceCompanion[A] =>
  
  implicit val resolver = self
  
  implicit object ReferenceReader extends BSONDocumentReader[Reference[A]] {
    def read(doc: BSONDocument): Reference[A] =
      Reference(
        doc.getAs[BSONObjectID]("reference_id").get
      )
      
  }

  implicit object ReferenceWriter extends BSONDocumentWriter[Reference[A]] {
    def write(reference: Reference[A]): BSONDocument =
      BSONDocument(
        "reference_id" -> reference.id
      )
  }
  
  // MapReader & MapWriter
  implicit def MapReferenceReader[V](implicit vr: BSONDocumentReader[V]): BSONDocumentReader[Map[String, V]] = new BSONDocumentReader[Map[String, V]] {
    def read(bson: BSONDocument): Map[String, V] = {
      val elements = bson.elements.map { tuple =>
        // assume that all values in the document are BSONDocuments
        tuple._1 -> vr.read(tuple._2.seeAsTry[BSONDocument].get)
      }
      elements.toMap
    }
  }

  implicit def MapWriter[V](implicit vw: BSONDocumentWriter[V]): BSONDocumentWriter[Map[String, V]] = new BSONDocumentWriter[Map[String, V]] {
    def write(map: Map[String, V]): BSONDocument = {
      val elements = map.toStream.map { tuple =>
        tuple._1 -> vw.write(tuple._2)
      }
      BSONDocument(elements)
    }
  }
  
}


trait RefPersistanceCompanion[T <: ModelObj] extends SafePersistanceCompanion[T]{
  
    override lazy val dbName = "vivathron"

    //def findOneByUniqueString(idStr: String) = findOneByIdString(idStr)  
    //def uniqueString(obj: T) = obj.id.stringify    
      
    def update(id: BSONObjectID, obj: T) = {
      val res1 = Promise[Boolean]
      val res2 = Promise[Boolean]
      val globalRes = Promise[Option[T]]
      
      (this) match {
        case me : SonPersistanceCompanion[T, _] => {
        	  me.updateUpOnUpdate(id,obj).onComplete{
        	    _ => res1.trySuccess(true)
        	  }
        	}
        case _ =>
          res1.trySuccess(true)
      }
      (this) match {
        case me : FatherPersistanceCompanion[T, _] => {
          me.updateDownOnUpdate(id,obj).onComplete{
        	    _ => res2.trySuccess(true)
        	  }
        }
        case _ =>
          res2.trySuccess(true)
      }
      
      for {
        r1 <- res1.future
        r2 <- res2.future
      } yield {
        this._update(id, obj).onComplete
        {
          case Success(s) => globalRes.success(s)
          case Failure(f) => globalRes.failure(f)
        }
      }
      globalRes.future
    }
    
    
    def create(obj: T) = {
      val res1 = Promise[Boolean]
      val res2 = Promise[Boolean]
      val globalRes = Promise[Option[T]]
      
      (this) match {
        case me : SonPersistanceCompanion[T, _] => {
        	  me.updateUpOnCreate(obj).onComplete{
        	    _ => res1.trySuccess(true)
        	  }
        	}
        case _ =>
          res1.trySuccess(true)
      }
      (this) match {
        case me : FatherPersistanceCompanion[T, _] => {
          me.updateDownOnCreate(obj).onComplete{
        	    _ => res2.trySuccess(true)
        	  }
        }
        case _ =>
          res2.trySuccess(true)
      }
      
      for {
        r1 <- res1.future
        r2 <- res2.future
      } yield {
        this._create(obj).onComplete
        {
          case Success(s) => globalRes.success(s)
          case Failure(f) => globalRes.failure(f)
        }
      }
      globalRes.future
    }
    
    
    def delete(id: BSONObjectID) = {
      val res1 = Promise[Boolean]
      val res2 = Promise[Boolean]
      val globalRes = Promise[Boolean]
      
      (this) match {
        case me : SonPersistanceCompanion[T, _] => {
        	  me.updateUpOnDelete(id).onComplete{
        	    _ => res1.trySuccess(true)
        	  }
        	}
        case _ =>
          res1.trySuccess(true)
      }
      (this) match {
        case me : FatherPersistanceCompanion[T, _] => {
          me.updateDownOnDelete(id).onComplete{
        	    _ => res2.trySuccess(true)
        	  }
        }
        case _ =>
          res2.trySuccess(true)
      }
      
      for {
        r1 <- res1.future
        r2 <- res2.future
      } yield {
        this._delete(id).onComplete
        {
          case Success(s) => globalRes.success(s)
          case Failure(f) => globalRes.failure(f)
        }
      }
      globalRes.future
      
    }

}
