package object models {

  import models.persistence.PersistenceCompanion
  import scala.language.implicitConversions

  implicit def makeReference[T <: ModelObj](x: T)(implicit resolver: PersistenceCompanion[T]): Reference[T] =
    Reference(x.myId)
}
