package models.persistance

import reactivemongo.bson._
import MongoDBsConnector._
import models._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Promise
import scala.concurrent.Future
import akka.actor._
import models.persistance.ReactiveMongoActorSystem._

trait SafePersistanceCompanion[T <: ModelObj] extends PersistanceCompanion[T] {
  me =>    
    def findOneByUniqueString(str: String): Future[Option[T]]
    
    def uniqueString(obj: T): String 
    
    lazy val actor = system.actorOf(Props(CollectionManager()),collectionName+"Manager")
      
     /*
     * CREATE -> executed only once
     * UPDATE -> sequentially applied
     * DELETE -> executed next and remove the queue --> now not used and only partial implementation
     */
    abstract class Operation(prop: String, result: Promise[Boolean]) {
      val uuid = BSONObjectID.generate
      
      val getProp = prop
      
      def execute(implicit self: ActorRef) : Future[Any]
      
      def newQueue(old: List[Operation]): List[Operation]
      
      //helper for common use
      def postSuccesfull(): Unit = {}
      def postFailure(): Unit = {}
    }
 
    class Create(prop: String, obj: T)(result: Promise[Boolean]) extends Operation(prop, result) {
      def execute(implicit self: ActorRef): Future[Boolean] = {
             for (
                it <- me.findOneByUniqueString(prop)
                ) {
              it match {
              case None =>
              	me._createPC(obj) onComplete { 
              	  case Success(ok) =>
              	    postSuccesfull
              	    result.trySuccess(true)
            	  case _ =>
            	    postFailure
            		result.trySuccess(false)
              	}
              case _ =>
                postFailure
                result.trySuccess(false)
              }
             }
        result.future
      }
      def newQueue(old: List[Operation]): List[Operation] = {
        if (old.isEmpty)
          List(this)
        else
          if (old.find(
                _.isInstanceOf[Create]).isEmpty
                )
            	old.::(this)
            else 
            	old
      }
    }
    
    class Update(prop: String, params: Tuple2[BSONObjectID, T])(result: Promise[Boolean]) extends Operation(prop,result) {
      def execute(implicit self: ActorRef): Future[Boolean] = {
             for (
                it <- me.findOneByUniqueString(prop)
                ) {
              it match {
              case Some(_) =>
              	me._updatePC(params._1,params._2) onComplete { 
              	  case Success(ok) =>
              	    postSuccesfull
              	    result.trySuccess(true)
            	  case _ =>
            	    postFailure
            		result.trySuccess(false)
              	}
              case _ =>
                postFailure
                println("SafePC "+collectionName+" => trying to update on "+collectionName+" but is not created")
                result.trySuccess(false)
              }
             }
        result.future
      }
      def newQueue(old: List[Operation]): List[Operation] = {
           old.::(this)
      }
    }
    
    class Delete(prop: String, id: BSONObjectID)(result: Promise[Boolean]) extends Operation(prop, result) {
      def execute(implicit self: ActorRef): Future[Boolean] = {
              	me._deletePC(id) onComplete { 
              	  case Success(ok) =>
              	    postSuccesfull
              	    result.trySuccess(true)
            	  case _ =>
            	    postFailure
            		result.trySuccess(false)
              	}
        result.future
      }
      def newQueue(old: List[Operation]): List[Operation] = {
           List(this)
      }
    }
    
    sealed case class DoNextOn(prop: String, uuid: BSONObjectID)
    
    
    case class CollectionManager() extends Actor {
      
    def receive = {
      operative(Map())
    }
    
    import scala.util.{Failure, Success}
    def operative(running: Map[String , List[Operation]]): Receive = {

       case DoNextOn(prop,uuid) => {
      		running.map(o =>
      		  if (o._1.equals(prop)) {
      		    val op = o._2.filter(e => e.uuid==uuid)
      		    val nexts = o._2.filterNot(e => e.uuid==uuid)
      		    if(op.size==0)
      		    	println("SafePC "+collectionName+" => DoNextOn: cannot execute "+o._2+" because it doesn't exist...")
      		    else
      		      op.headOption.map(op => {
      		        op.execute onComplete {
      		          case _ => {
      		            context.become(operative(updateOperations(running)(o._1,nexts)), true)
      		            if(nexts.size>0)
      		            	nexts.headOption.map(o => self ! DoNextOn(o.getProp, o.uuid))
      		            else
      		            	println("SafePC "+collectionName+" => DoNextOn: done!")
      		          }
      		        }
      		      }
      		    )
      		  }
      		  else
      		    println("SafePC "+collectionName+" => DoNextOn: not existent prop...")
      	   )
      }		  
      case o: Operation => {
        val newoperations =
        	running.find(p => p._1.equals(o.getProp)).map(q =>
        	  		o.newQueue(q._2)
        	  ).getOrElse(        	        
        			o.newQueue(List())
        	  )	  
        context.become(operative(addOperation(running)(o.getProp,newoperations)), true)
      }
    }
    
    def addOperation(running: Map[String , List[Operation]])(prop: String, os: List[Operation]) = {
        if (running.contains(prop)) {
            running.map(p =>
            if (p._1.equals(prop))
            {
              if (p._2.isEmpty && os.headOption.isDefined) 
                self ! DoNextOn(os.head.getProp, os.head.uuid)
                
              prop -> os
            } else p
          )
        }
        else {
        	if (os.headOption.isDefined)
        		self ! DoNextOn(os.head.getProp, os.head.uuid)
        		
            running + (prop -> os)
        }
    }
    
    def updateOperations(running: Map[String , List[Operation]])(prop: String, os: List[Operation]) = 
      		if(os.size>0)
      		  running.map(o =>
      		  	if (o._1.equals(prop)) prop -> os
      		  	else o)
      		else
      		  running.filter(o => !o._1.equals(prop))
    
    
}

   override def _create(obj: T) = {
     val result = Promise[Boolean]
     println("SafePC "+collectionName+" => _create")
     actor ! new Create(uniqueString(obj),obj)(result)
     result.future.map(res => if (res) Some(obj) else None)    
   }

   protected def _createPC(obj: T) = {
     super._create(obj)
   }
  
  override def _update(id: BSONObjectID,obj: T): Future[Option[T]] = {
    val result = Promise[Boolean]
    println("SafePC "+collectionName+" => _update")
    actor ! new Update(uniqueString(obj),(id,obj))(result)
    result.future.map(res => if (res) Some(obj) else None)
  }
  
  protected def _updatePC(id: BSONObjectID,obj: T) = {
    super._update(id,obj)
  }
  
  override def _delete(id: BSONObjectID): Future[Boolean] = {
    val result = Promise[Boolean]
    println("SafePC "+collectionName+" => _delete")
    for{
      obj <- findOneById(id)
    }yield{
      if(obj.isDefined)
    	  actor ! new Delete(uniqueString(obj.get),id)(result)
      else
          println("SafePC "+collectionName+" => element to delete not found")
    }
    result.future
  }
  
  protected def _deletePC(id: BSONObjectID) = {
    super._delete(id)
  }
  
}
