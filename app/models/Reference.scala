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

case class Reference[T <: ModelObj](id: BSONObjectID) {
    
}


trait ReferenceJSONer[A <: ModelObj] {
  
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
  
}


trait RefPersistanceCompanion[T <: ModelObj] extends PersistanceCompanion[T]{
  
    def update(id: BSONObjectID, obj: T) = {
      logger.debug("REF PERSISTANCE COMPANION UPDATE")
      val res1 = Promise[Boolean]
      val res2 = Promise[Boolean]
      val globalRes = Promise[Option[T]]
      
      (this) match {
        case me : ReverseRefPersistanceCompanion[T, _] => {
        	  me.updateUpOnUpdate(id,obj).onComplete{
        	    _ => res1.trySuccess(true)
        	  }
        	}
        case _ =>
          res1.trySuccess(true)
      }
      (this) match {
        case me : DirectRefPersistanceCompanion[T, _] => {
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
        super._update(id, obj).onComplete
        {
          case Success(s) => globalRes.success(s)
          case Failure(f) => globalRes.failure(f)
        }
      }
      globalRes.future
    }
    
    
    def create(obj: T) = {
      logger.debug("REF PERSISTANCE COMPANION CREATE")
      val res1 = Promise[Boolean]
      val res2 = Promise[Boolean]
      val globalRes = Promise[Option[T]]
      
      (this) match {
        case me : ReverseRefPersistanceCompanion[T, _] => {
        	  me.updateUpOnCreate(obj).onComplete{
        	    _ => res1.trySuccess(true)
        	  }
        	}
        case _ =>
          res1.trySuccess(true)
      }
      (this) match {
        case me : DirectRefPersistanceCompanion[T, _] => {
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
        super._create(obj).onComplete
        {
          case Success(s) => globalRes.success(s)
          case Failure(f) => globalRes.failure(f)
        }
      }
      globalRes.future
    }
    
    
    def delete(id: BSONObjectID) = {
      logger.debug("REF PERSISTANCE COMPANION DELETE")
      val res1 = Promise[Boolean]
      val res2 = Promise[Boolean]
      val globalRes = Promise[Boolean]
      
      (this) match {
        case me : ReverseRefPersistanceCompanion[T, _] => {
        	  me.updateUpOnDelete(id).onComplete{
        	    _ => res1.trySuccess(true)
        	  }
        	}
        case _ =>
          res1.trySuccess(true)
      }
      (this) match {
        case me : DirectRefPersistanceCompanion[T, _] => {
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
        super._delete(id).onComplete
        {
          case Success(s) => globalRes.success(s)
          case Failure(f) => globalRes.failure(f)
        }
      }
      globalRes.future
      
    }

}


trait ReverseRefPersistanceCompanion[T <: ModelObj, R <: ModelObj] { 

	def referenceChanged: ((Option[Reference[R]], Reference[T]) => Future[Boolean])
	
	// links management methos signatures used in performing crud ops
	def updateUpOnUpdate(id: BSONObjectID, obj: T): Future[Boolean]
	
	def updateUpOnCreate(obj: T): Future[Boolean]
	
	def updateUpOnDelete(id: BSONObjectID): Future[Boolean]
    
}


trait DirectRefPersistanceCompanion[T <: ModelObj, R <: ModelObj] {
  
	def removeFrom(toBeRemoved: List[Reference[R]], from: List[T]): Future[Boolean]
	
	def addTo(toBeAdded: List[Reference[R]], to: T): Future[Boolean]
	
	// links management methos signatures used in performing crud ops
	def updateDownOnUpdate(id: BSONObjectID, obj: T): Future[Boolean]
	
	def updateDownOnCreate(obj: T): Future[Boolean]
	
	def updateDownOnDelete(id: BSONObjectID): Future[Boolean]
  
}
