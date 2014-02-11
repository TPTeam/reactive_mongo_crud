package object models {
	import models.persistance.PersistanceCompanion
	import scala.language.implicitConversions
    implicit def makeReference[T <: ModelObj](x: T)(implicit resolver: PersistanceCompanion[T]): Reference[T] = 
    		Reference(x.myId)
  
}