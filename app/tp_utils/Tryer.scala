package tp_utils

import scala.concurrent.Future
import scala.concurrent.Promise
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.Failure
import scala.util.Success

/**
 * Object to be used in order to import helper on "try catch"
 */
object Tryer {

  def tryo[T <: Any](f: => T): Option[T] =
    try
      Some(f)
    catch {
      case _: Throwable => None
    }

  def checko[T <: Any](f: => Boolean): Boolean =
    try
      f
    catch {
      case _: Throwable => false
    }

}

object Futurizer {
  
  def futo[T <: Any](f: => Future[T]): Future[T] = {
    val res = Promise[T]
    f.onComplete
    {
      case Success(s) => res.success(s)
      case Failure(f) => res.failure(f)
    }
    res.future
  }
    
  
}