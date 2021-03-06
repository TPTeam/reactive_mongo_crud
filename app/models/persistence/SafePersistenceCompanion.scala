package models.persistence

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
import models.persistence.ReactiveMongoActorSystem._

trait SafePersistenceCompanion[T <: ModelObj] extends PersistenceCompanion[T] {
  me =>
  def findOneByUniqueId(id: BSONObjectID): Future[Option[T]] =
    findOneById(id)

  def uniqueId(obj: T): BSONObjectID =
    obj.id

  lazy val actor = system.actorOf(Props(CollectionManager()),me.dbName+collectionName+"Manager")

  /*
  * CREATE -> executed only once
  * UPDATE -> sequentially applied
  * DELETE -> executed next and remove the queue --> now not used and only partial implementation
  */
  abstract class Operation(prop: BSONObjectID, result: Promise[Boolean]) {
    val uuid = BSONObjectID.generate

    val getProp = prop

    def execute(implicit self: ActorRef) : Future[Any]

    def newQueue(old: List[Operation]): List[Operation]

    //helper for common use
    def postSuccesfull(): Unit = {}
    def postFailure(): Unit = {}
  }

  class Create(obj: T)(result: Promise[Boolean]) extends Operation(obj.id, result) {
    def execute(implicit self: ActorRef): Future[Boolean] = {
      for (
        it <- me.findOneByUniqueId(obj.id)
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

  class Update(params: Tuple2[BSONObjectID, T])(result: Promise[Boolean]) extends Operation(params._1,result) {
    def execute(implicit self: ActorRef): Future[Boolean] = {
      for (
        it <- me.findOneByUniqueId(params._1)
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

  class Delete(id: BSONObjectID)(result: Promise[Boolean]) extends Operation(id, result) {
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

  sealed case class DoNextOn(prop: BSONObjectID, uuid: BSONObjectID)

  case class CollectionManager() extends Actor {

    def receive = {
      operative(Map())
    }

    import scala.util.{Failure, Success}
    def operative(running: Map[BSONObjectID , List[Operation]]): Receive = {

      case DoNextOn(prop,uuid) => {
        val newrunning =
          running.map(o =>
            if (o._1 == prop) {
              val nexts = o._2.filterNot(e => e.uuid==uuid)
              nexts.headOption.map(o => {
                o.execute onComplete {
                  case _ =>
                    self ! DoNextOn(o.getProp, o.uuid)
                }}
              )
              prop -> nexts
            }
            else o)
        val newrunningOptimized =
          newrunning.filter(x => !x._2.isEmpty)
        context.become(operative(newrunningOptimized), true)
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

    def addOperation(running: Map[BSONObjectID , List[Operation]])(prop: BSONObjectID, os: List[Operation]) = {
      if (running.contains(prop)) {
        running.map(p =>
          if (p._1.equals(prop))
          {
            if (p._2.isEmpty && os.headOption.isDefined)
              self ! DoNextOn(os.head.getProp, BSONObjectID.generate)

            prop -> os
          } else p
        )
      }
      else {
        if (os.headOption.isDefined)
          self ! DoNextOn(os.head.getProp, BSONObjectID.generate)
        else
          self ! DoNextOn(prop, BSONObjectID.generate)

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

  def _create(obj: T) = {
    val result = Promise[Boolean]
    //println("SafePC "+collectionName+" => _create")
    actor ! new Create(obj)(result)
    result.future.map(res => if (res) Some(obj) else None)
  }

  protected def _createPC(obj: T) = {
    originalCreate(obj)
  }

  def _update(id: BSONObjectID,obj: T): Future[Option[T]] = {
    val result = Promise[Boolean]
    //println("SafePC "+collectionName+" => _update")
    actor ! new Update((id,obj))(result)
    result.future.map(res => if (res) Some(obj) else None)
  }

  protected def _updatePC(id: BSONObjectID,obj: T) = {
    originalUpdate(id,obj)
  }

  def _delete(id: BSONObjectID): Future[Boolean] = {
    val result = Promise[Boolean]
    //println("SafePC "+collectionName+" => _delete")
    for{
      obj <- findOneById(id)
    }yield{
      if(obj.isDefined)
        actor ! new Delete(id)(result)
      else
        println("SafePC "+collectionName+" => element to delete not found")
    }
    result.future
  }

  protected def _deletePC(id: BSONObjectID) = {
    originalDelete(id)
  }

}