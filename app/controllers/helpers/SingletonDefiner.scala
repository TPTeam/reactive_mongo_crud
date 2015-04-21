package controllers.helper

trait SingletonDefiner[C <: models.ModelObj] {

  val singleton: models.RefPersistenceCompanion[C]

  lazy val obj = singleton

}