package tp_utils

import play.api.libs.json._

object Jsoner {
  
  import play.api.libs.json.Writes._
  def toJsVal(i: Int): JsValue =
          IntWrites.writes(i)
  def toJsVal(l: Long): JsValue =
          LongWrites.writes(l)
  def toJsVal(d: Double): JsValue =
          DoubleWrites.writes(d)
  
}