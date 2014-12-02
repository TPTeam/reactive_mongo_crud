package controllers.helper

trait SingletonDefiner[C <: models.ModelObj] {
	
	val singleton: models.RefPersistanceCompanion[C]
	
	lazy val obj = singleton

}