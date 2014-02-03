package models

import persistance._
import reactivemongo.bson._

abstract class ModelObj(_id: BSONObjectID) {
  type C <: ModelObj
  type PC = RefPersistanceCompanion[_]
  
  def myId = _id 
  
  val singleton: PC
  
}