package controllers

import play.api.data._
import play.api.mvc._
import tp_utils.Tryer._
import play.api.libs.json._
import reactivemongo.bson._
import scala.language.implicitConversions
import scala.util.Success
import play.Logger


package object controllerhelper {

  implicit def fromMappingToVerifiedId(x: Mapping[String]) =
    new {
	  def verifyId:  play.api.data.Mapping[String] =
	    x.verifying(id => {
	       checko({new BSONObjectID(id);true})
	    })
  	}
  
  
  implicit def fromMappingToVerifiedOptionBSONId(x: Mapping[String]) =
    new {
	  def verifyOptionBSONId:  play.api.data.Mapping[String] =
	    x.verifying(id =>
	     if (id.trim.equals(""))
	       true
	     else
	    	BSONObjectID.parse(id) match
	    	{
	    		case Success(s) => true
	    		case _ => false
	    	}
	    )
  	}
  
    
  //TO verify
  implicit def fromMappingToVerifiedIdOption(x: Mapping[String]) =
    new {
	  def verifyOptId:  play.api.data.Mapping[String] =
	    x.verifying(id => {
	     if (id.trim.equals(""))
	       true
	     else
	       checko({new BSONObjectID(id);true})
	    })
  	}
  
  implicit def fromMappingToVerifiedJson(x: Mapping[String]) =
    new {
	  def verifyOptJson:  play.api.data.Mapping[String] =
	    x.verifying(data => {
	     if (data.trim.equals(""))
	       true
	     else
	       checko(Json.parse(data)!=JsNull)
	    })
  	}
  
  def checkRemove(implicit request: Request[AnyContent]) = 
		  (for {
            elems <- request.body.asFormUrlEncoded
            values <- elems.get("remove")
            value <- values.headOption
          } yield {
            (checko(value.toBoolean) || checko(value.equals("on")))
          }).getOrElse(false)
  
}