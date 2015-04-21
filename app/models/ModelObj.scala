package models

import persistence._
import reactivemongo.bson._

abstract class ModelObj(_id: BSONObjectID) {
  type C <: ModelObj
  type PC = RefPersistenceCompanion[_]

  def myId = _id

  val singleton: PC

}