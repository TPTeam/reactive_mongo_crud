package object models {
	import scala.language.implicitConversions
    implicit def makeReference[T <: ModelObj](x: T): Reference[T] = 
    		Reference(x.myId)
  
}