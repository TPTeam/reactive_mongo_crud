package models.persistance

import models.ModelObj
import models._
import reactivemongo.bson.BSONObjectID
import reactivemongo.bson.BSONDocument
import scala.util.{Success, Failure}
import scala.concurrent._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.language.implicitConversions

trait FatherPersistanceCompanion[T <: ModelObj, R <: ModelObj] {
  self: RefPersistanceCompanion[T]=>  
  
  val CHILD: PersistanceCompanion[R] with SonPersistanceCompanion[R,T] 
  val sonsAttName: String
  
  def getSons(obj: T): List[Reference[R]]
  
  //def removeFrom(toBeRemoved: List[Reference[R]], from: List[T]): Future[Boolean]
  protected[models] def removeFrom(toBeRemoved: List[Reference[R]], from: List[Reference[T]]): Future[Boolean]
  
  protected[models] def cleanChildren(toBeRemoved: List[Reference[R]], from: List[Reference[T]]): Future[Boolean] = {
	val r = Promise[Boolean]
    val rIds = toBeRemoved.map(x => BSONDocument("reference_id" -> x.id))
	val fIds = from.map(x => x.id)
    collection.update(BSONDocument("_id" -> BSONDocument( "$in" -> fIds)), 
                      BSONDocument("$pullAll" -> BSONDocument(sonsAttName -> rIds)),
                      multi=true).onComplete{
      case Success(s) => r.trySuccess(true)
      case Failure(f) => r.trySuccess(false)
    }
    r.future
  }      

  //def addTo(toBeAdded: List[Reference[R]], to: T): Future[Boolean]
  protected[models] def addTo(toBeAdded: List[Reference[R]], to: Reference[T]): Future[Boolean]
  
  protected[models] def addChildren(toBeAdded: List[Reference[R]], to: Reference[T]): Future[Boolean] = {
    val r = Promise[Boolean]
    val rIds = toBeAdded.map(x => BSONDocument("reference_id" -> x.id))
    collection.update(BSONDocument("_id" -> to.id), 
                      BSONDocument("$addToSet" -> BSONDocument(sonsAttName -> 
                      BSONDocument("$each" -> rIds)))).onComplete{
      case Success(s) => r.trySuccess(true)
      case Failure(f) => r.trySuccess(false)
    }
    r.future
  }
  
  private[models] def updateDownOnCreate(obj: T) = {  
    val overallBlock = Promise[Boolean]
    val removeFromGPBlock = Promise[Boolean]
    val updateSonsBlock = getSons(obj).map( x => Promise[Boolean])
    
    for {
      granpas <- findAllIdsWithFilter(BSONDocument("_id" -> BSONDocument( "$nin" -> List(obj.id)))).collect[List]()
      //granpas <- findAll.collect[List]()
    } yield {
    	removeFrom(getSons(obj),granpas.map(i => Reference(i))).onComplete{
    	  _ => removeFromGPBlock.trySuccess(true)
    	} 
    	getSons(obj).zipWithIndex.foreach(x => {
    	  CHILD.referenceChanged(Some(new Reference[T](obj.id)),x._1).onComplete{
    	  _ => updateSonsBlock(x._2).trySuccess(true)}
    	}) 
    	val updateSonsRes = Future.fold(updateSonsBlock.map(x => x.future))(true)((i, l) => l)
    	
    	for{
    	  res1 <- removeFromGPBlock.future
    	  res2 <- updateSonsRes
    	}yield{
    	  overallBlock.trySuccess(true)
    	}	
    }
    overallBlock.future
  }
  
  
  private[models] def updateDownOnDelete(id: BSONObjectID) = {
        
    val overallBlock = Promise[Boolean]
    for {
      g <- findOneById(id) 
    } yield {
      if (g.isEmpty) overallBlock.trySuccess(true)
      else {
    	  val sons = getSons(g.get)
    	  val updateSonsBlock = sons.map( _ => Promise[Boolean])
    	  for(s <- sons.zipWithIndex)
    		  CHILD.referenceChanged(None,s._1).onComplete{
    		  	_ => updateSonsBlock(s._2).trySuccess(true)
    		  } 
    	  val updateSonsRes = Future.fold(updateSonsBlock.map(x => x.future))(true)((i, l) => l)
    	  for{
    		  x <- updateSonsRes
    	  }yield{
    		  overallBlock.trySuccess(true)
    	  }	
      }
    }
    overallBlock.future
  }
    
  
  private[models] def updateDownOnUpdate(id: BSONObjectID, obj: T) = {
    val overallBlock = Promise[Boolean]
    val removeFromGPBlock = Promise[Boolean]
    val updateFathersBlock = Promise[Boolean]
    
    for {
      granpas <- findAllIdsWithFilter(BSONDocument("_id" -> BSONDocument( "$nin" -> List(obj.id)))).collect[List]()
      //granpas <- findAll.collect[List]()
    } yield{ // remove the new granpa sons from the other granpas
    	removeFrom(getSons(obj), granpas.map(i => Reference(i))).onComplete{
    	  _ => removeFromGPBlock.trySuccess(true)
    	} 
    	
    	for {
    		gp <- findOneById(id)
    	} yield {
    		val olds = getSons(gp.get)
    		val news = getSons(obj)																																																																																																										
    		val oldsForBlock = olds.map( _ => Promise[Boolean] )
    		val newsForBlock = news.map( _ => Promise[Boolean] )
    		
    		for (o <- olds.zipWithIndex)
    			if (!news.exists(x => x.id==o._1.id)) //delete fathers from the old
    				CHILD.referenceChanged(None,o._1).onComplete{
    					_ => oldsForBlock(o._2).trySuccess(true)
    			}
    			else
    				oldsForBlock(o._2).trySuccess(true)
    	  
    		for (n <- news.zipWithIndex)
    			if (!olds.exists(x => x.id == n._1.id)) //add new 
    				CHILD.referenceChanged(Some(Reference(id)),n._1).onComplete{
    					_ => newsForBlock(n._2).trySuccess(true)
    			}
    			else
    				newsForBlock(n._2).trySuccess(true)
		
    		val resOldSons = Future.fold(oldsForBlock.map(x => x.future))(true)((i, l) => l)
    		val resNewSons = Future.fold(newsForBlock.map(x => x.future))(true)((i, l) => l)
    	
    		for{	//finally update gp
    			block1 <- removeFromGPBlock.future 
    			block2 <- resOldSons 
    			block3 <- resNewSons
    		}yield{
    			overallBlock.trySuccess(true)
    		}
    	}
        
    }
    overallBlock.future
  }
  
}