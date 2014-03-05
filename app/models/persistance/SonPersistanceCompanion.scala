package models.persistance

import models.ModelObj
import reactivemongo.bson.BSONObjectID
import scala.concurrent.Promise
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.language.implicitConversions
import models.Reference
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import play.api.Logger._
import reactivemongo.bson.BSONDocumentReader
import reactivemongo.bson.BSONDocument


trait SonPersistanceCompanion[T <: ModelObj, R <: ModelObj] {
	self: PersistanceCompanion[T] =>
		  
	type FATHER = PersistanceCompanion[R] with FatherPersistanceCompanion[R,T]   
	
	val FatherPC: FATHER
	
	def getFather(obj: T): Option[Reference[R]]	
	
	def referenceChanged: ((Option[Reference[R]], Reference[T]) => Future[Boolean])
	
	def FatherReferenceReader(field: String) = 
	  new BSONDocumentReader[Reference[R]] {
		def read(doc: BSONDocument): Reference[R] = {
    		doc.getAs[Reference[R]](field)(FatherPC.ReferenceReader).get
		}
	  }
	
	def updateUpOnCreate(obj: T) = {
		val overallBlock = Promise[Boolean]
		if (getFather(obj).isDefined)
			for {
				fa <- FatherPC.findOneById(getFather(obj).get.id)
			} yield{ 
				FatherPC.addTo(List(new Reference[T](obj.id)), fa.get)
			}.onComplete{
				case _ => overallBlock.trySuccess(true)
			}
		else
			overallBlock.trySuccess(true)
		overallBlock.future
	}
	
	
	def updateUpOnDelete(id: BSONObjectID) = {
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
		       fa <- FatherPC.findOneById(getFather(obj.get).get.id)
		     }
		     yield{
		       if (fa.isEmpty) overallBlock.trySuccess(true)
		       else 
		         FatherPC.removeFrom(List(new Reference[T](id)), List(fa.get)).onComplete{
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
	
	
	def updateUpOnUpdate(id: BSONObjectID, obj: T) = {
		val fathersRemoveFromBlock = Promise[Boolean]
		val fathersAddToBlock = Promise[Boolean]
		val overallBlock = Promise[Boolean]
    
		for {
			fathers <- FatherPC.findAll.collect[List]()
		}
		yield
		{	// update fathers updating son references
			if (fathers.size == 0) {
			  fathersRemoveFromBlock.trySuccess(true)
			  fathersAddToBlock.trySuccess(true)
			} else {
			FatherPC.removeFrom(List(Reference[T](id)), fathers).onComplete{
				_ => fathersRemoveFromBlock.trySuccess(true)
			}
			if(getFather(obj).isDefined)
				for{
					fathOpt <- FatherPC.findOneById(getFather(obj).get.id)
				}
				yield{
					FatherPC.addTo(List(Reference[T](obj.id)), fathOpt.get).onComplete{ 
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