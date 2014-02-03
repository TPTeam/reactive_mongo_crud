package tp_utils

import play.api.libs.json._

object deJsoner {

  def getString(obj: JsValue, name: String): String =
    obj.\(name) match {
      case sid: JsString => sid.value
      case _ => obj.\(name).toString
    }
}