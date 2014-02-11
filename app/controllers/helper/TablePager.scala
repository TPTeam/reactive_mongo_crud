package controllers

import play.api.mvc._
import play.api.libs.concurrent._
import models.persistance._
import play.api.libs.json._
import tp_utils.Tryer._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import Json._
import scala.concurrent.Future
import models._
import reactivemongo.bson._
import reactivemongo.api._

trait TablePager[C <: ModelObj] extends SingletonDefiner[C] {
  self: Controller =>
  
	/*
	 * To implement or to
	 * override
	 */
    val elemsToDisplay = Seq("name","description")
    def elemReader(keys: Seq[String])(doc: BSONDocument): Seq[String] = {
      keys.map(k =>
      	if (k=="id")
      		doc.getAs[BSONObjectID]("_id").get.stringify
        else
        	doc.getAs[String](k).get
      )
    }
    val defaultDisplayLenth: Long = 10
	val defaultSortBy = "name"
	  
	def SeqObjReader(keys: Seq[String]) = new BSONDocumentReader[Seq[String]] {
      def read(doc: BSONDocument): Seq[String] = {
        elemReader(keys)(doc)
      }
    }
    
	def sortBy(implicit params: Map[String,Seq[String]]) =
		  tryo(elemsToDisplay(sortCol.toInt)).getOrElse(defaultSortBy)
	
    def filter(implicit params: Map[String,Seq[String]]) = 
      	params.get("sSearch").getOrElse(Seq("")).head
    def pageSize(implicit params: Map[String,Seq[String]]) =
      	Integer.valueOf(params.get("iDisplayLength")
    				.getOrElse(Seq(""+defaultDisplayLenth)).head)
    def page(implicit params: Map[String,Seq[String]]) =
        Integer.valueOf(params.get("iDisplayStart")
    			.getOrElse(Seq("0")).head) / pageSize
    def order(implicit params: Map[String,Seq[String]]) = 
      if (params.get("sSortDir_0").getOrElse(Seq("asc")).head.compareTo("desc")==0)
    				-1 else 1
    def sortCol(implicit params: Map[String,Seq[String]]) =
      params.get("sSortCol_0").getOrElse(Seq("asc")).head
    
    def sEcho(implicit params: Map[String,Seq[String]]) =
      Integer.valueOf(params.get("sEcho").getOrElse(Seq("0")).head)
    
    def start(implicit params: Map[String,Seq[String]]) =
      (page)*pageSize 
    
    def filt(implicit params: Map[String,Seq[String]]) =
      BSONRegex("(?i).*"+filter+".*","")

    def objsWithQuery(implicit params: Map[String,Seq[String]]) =
      obj.find(BSONDocument(
          "$or" -> elemsToDisplay.map(elem => BSONDocument(elem -> filt))),	//filter
          BSONDocument.apply(												//projection
              for{elem <- elemsToDisplay} yield {
            	  elem -> BSONBoolean(true)
              }))
    
    import scala.language.postfixOps
    def actualPage(iTotalRecords: Int)(implicit params: Map[String,Seq[String]]) = {
      objsWithQuery
            .sort(BSONDocument(sortBy -> order))
            .options(
            		reactivemongo.api.QueryOpts(skipN = start, batchSizeN = //pageSize
            				  {
            				      if ((start + pageSize) > iTotalRecords) (pageSize - start)
            					  else pageSize
            				  }
            		  ))
    }
    def table = Action.async { implicit request ⇒
        implicit val params = request.queryString
        
            /**
             * 	Construct the JSON to return
             */
            import Json._
            import tp_utils.Jsoner._

            for {
              iTotalRecords <- singleton.count;
              ap <- actualPage(iTotalRecords).cursor[Seq[String]](SeqObjReader(elemsToDisplay), defaultContext).collect[List]()
            } yield {
            Ok(
            JsObject(
            		Seq(
            				"sEcho" -> toJsVal(sEcho),
            				"iTotalRecords" -> toJsVal(iTotalRecords),
            				"iTotalDisplayRecords" -> toJsVal(ap.size),
            				"aaData" -> 
            				JsArray(
            						ap.map(elem => 
            			  JsObject(
            			      elemsToDisplay.zipWithIndex.map(e =>
            			      		e._2.toString -> toJson(elem(e._2))
                            ))
                       ).toSeq)   
          )
        ))
     }
  }
}