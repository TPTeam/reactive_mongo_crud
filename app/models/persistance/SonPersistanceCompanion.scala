package models.persistance

import models.ModelObj
import reactivemongo.bson.BSONObjectID
import scala.concurrent.Promise
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.language.implicitConversions
import models.Reference
import models.ReferenceJSONer
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import play.api.Logger._
import reactivemongo.bson.BSONDocumentReader
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.BSONDocumentWriter
import models.RefPersistanceCompanion


trait SonPersistanceCompanion[T <: ModelObj, R <: ModelObj] {
	self: RefPersistanceCompanion[T] =>
		  
	type FATHER = PersistanceCompanion[R] with FatherPersistanceCompanion[R,T]   
	
	val FatherPC: FATHER
	val fatherAttName: String
	
	def getFather(obj: T): Option[Reference[R]]	
	
	def findByFatherReference(fRef: Reference[R]) = {
     collection.find(BSONDocument(
          fatherAttName -> BSONDocument(
              "reference_id" -> fRef.id
              )
          ), BSONDocument("id" -> 1 , "name" -> 1)).cursor(idStringReader("name"), defaultContext)
    }
	
	protected[models] def updateFather(rel: Reference[T], gp: Reference[R]): Future[Boolean] = {
	  val r = Promise[Boolean]
	  collection.update(BSONDocument("_id" -> rel.id), 
                      BSONDocument("$set" -> BSONDocument(fatherAttName -> 
                      BSONDocument("reference_id" -> gp.id)))).onComplete{
           case Success(s) => r.trySuccess(true)
           case Failure(f) => r.trySuccess(false)
	  }
	  r.future	  
	}

	
	protected[models] def referenceChanged(ogp: Option[Reference[R]],rel: Reference[T]): Future[Boolean]
	  
	def FatherReferenceReader(field: String) = 
	  new BSONDocumentReader[Reference[R]] {
		def read(doc: BSONDocument): Reference[R] = {
    		doc.getAs[Reference[R]](field)(FatherPC.ReferenceReader).get
		}
	  }
	
	private[models] def updateUpOnCreate(obj: T) = {
		val overallBlock = Promise[Boolean]
		if (getFather(obj).isDefined)
			for {
				fa <- FatherPC.findOneIdById(getFather(obj).get.id)
			} yield{ 
				FatherPC.addTo(List(new Reference[T](obj.id)), Reference[R](fa.get)(FatherPC))
			}.onComplete{
				case _ => overallBlock.trySuccess(true)
			}
		else
			overallBlock.trySuccess(true)
		overallBlock.future
	}
	
	
	private[models] def updateUpOnDelete(id: BSONObjectID) = {
		val overallBlock = Promise[Boolean]

	    for {
			obj <- findOneById(id)
		} yield {
		 if (obj.isEmpty) {
		   overallBlock.trySuccess(true)   
		 } else 
		 obj.map(x => {
		   if(getFather(x).isDefined)
		   {
		     for{
		       fa <- FatherPC.findOneIdById(getFather(obj.get).get.id)
		     }
		     yield{
		       if (fa.isEmpty) overallBlock.trySuccess(true)
		       else 
		         FatherPC.removeFrom(List(new Reference[T](id)), List(Reference[R](fa.get)(FatherPC))).onComplete{
		     		case _ => overallBlock.trySuccess(true)
		       	}
		     }
		   }
		   else
			overallBlock.trySuccess(true)     
		 })
		}
		overallBlock.future
	}
	
	
	private[models] def updateUpOnUpdate(id: BSONObjectID, obj: T) = {
		val fathersRemoveFromBlock = Promise[Boolean]
		val fathersAddToBlock = Promise[Boolean]
		val overallBlock = Promise[Boolean]
    
		for {
			fathers <- FatherPC.findAllIds.collect[List]()
		}
		yield
		{	// update fathers updating son references
			if (fathers.size == 0) {
			  fathersRemoveFromBlock.trySuccess(true)
			  fathersAddToBlock.trySuccess(true)
			} else {
			FatherPC.removeFrom(List(Reference[T](id)), fathers.map(x => Reference[R](x)(FatherPC))).onComplete{
				_ => fathersRemoveFromBlock.trySuccess(true)
			}
			if(getFather(obj).isDefined)
				for{
					fathOpt <- FatherPC.findOneIdById(getFather(obj).get.id)
				}
				yield{
					FatherPC.addTo(List(Reference[T](obj.id)),  Reference[R](fathOpt.get)(FatherPC)).onComplete{ 
						_ => fathersAddToBlock.trySuccess(true)
					}
				}
			else
			fathersAddToBlock.trySuccess(true)
			}
		}
    
		for{
			b1 <- fathersRemoveFromBlock.future
			b2 <- fathersAddToBlock.future
		}yield{
			overallBlock.trySuccess(true)
		}
		overallBlock.future
	}
	
}