package controllers.helper

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

  val pagerLogVerbose = true

  def pagerLog(msg: String) = if (pagerLogVerbose) play.api.Logger.debug(s"[TablePager] : ${msg}")

  val elemsToDisplay = Seq("id")

  def elemReader(keys: Seq[String])(doc: BSONDocument): Seq[String] = {
    pagerLog(s"elemReader ${keys}")
    keys.map(k =>
      if (k == "id") {
        val res = doc.getAs[BSONObjectID]("_id")
        pagerLog(s"elemReader reading ${k} -> ${res}")
        res match {
          case Some(r) => r.stringify
          case _ => throw new Error("elementReader of TablePager cannot read 'id' field")
        }
      } else {
        val res = doc.getAs[String](k)
        pagerLog(s"elemReader reading ${k} -> ${res}")
        res match {
          case Some(r) => r
          case _ => throw new Error(s"elementReader of TablePager cannot read ${k} field: for non-string types you need to define your own deserializer for ${doc}")
        }
      })
  }
  val defaultDisplayLength: Int = 10
  val defaultSortBy = "id"

  def SeqObjReader(keys: Seq[String]) = new BSONDocumentReader[Seq[String]] {
    def read(doc: BSONDocument): Seq[String] = {
      pagerLog(s"SeqObjReader.read 	${keys}		${doc}")
      elemReader(keys)(doc)
    }
  }

  def sortBy(implicit params: Map[String, Seq[String]]) = {
//    pagerLog(s"sortBy		${sortCol.toInt}		${defaultSortBy}")
    tryo(elemsToDisplay(sortCol.toInt)).getOrElse(defaultSortBy)
  }

  def filter(implicit params: Map[String, Seq[String]]) = {
//    pagerLog(s"filter		${params.get("sSearch")}")
    params.get("sSearch").getOrElse(Seq("")).head
  }

  def pageSize(implicit params: Map[String, Seq[String]]): Int = {
//    pagerLog(s"pageSize		${params}")
    val size: Int = params.get("iDisplayLength") match {
      case Some(data) =>
        (try {
          if (data.length > 0) {
            Integer.valueOf(data.head)
          } else {
            play.api.Logger.warn(s"[TablePager] : iDisplayLength is not defined -> ${data}")
            defaultDisplayLength
          }
        } catch {
          case _: Throwable => {
            play.api.Logger.warn(s"[TablePager] : iDisplayLength is not a number -> ${data}")
            defaultDisplayLength
          }
        }) match {
          case number: Integer => number
          case _ => {
            play.api.Logger.warn(s"[TablePager] : parsed iDisplayLength but not a number")
            defaultDisplayLength
          }
        }
      case _ => {
        play.api.Logger.warn(s"[TablePager] : cannot find iDisplayLength")
        defaultDisplayLength
      }
    }
//    pagerLog(s"pageSize	is	${size}")
    size
  }
  
  def page(implicit params: Map[String, Seq[String]]): Int = {
//	pagerLog(s"page	with	${params}")
    if (pageSize > 0) {
      Integer.valueOf(params.get("iDisplayStart")
        .getOrElse(Seq("0")).head) / pageSize
    } else {
      0
    }
  }
	    
    def order(implicit params: Map[String,Seq[String]]): Int = 
      if (params.get("sSortDir_0").getOrElse(Seq("asc")).head.compareTo("desc")==0)
    				-1 else 1
    				
    def sortCol(implicit params: Map[String,Seq[String]]) =
      params.get("iSortCol_0").getOrElse(Seq("0")).head
    
    def sEcho(implicit params: Map[String,Seq[String]]) =
      Integer.valueOf(params.get("sEcho").getOrElse(Seq("0")).head)
    
    def start(implicit params: Map[String,Seq[String]]) =
      (page)*pageSize 
    
    def filt(implicit params: Map[String,Seq[String]]) =
      BSONRegex("(?i).*"+filter+".*","")
      
    def filterQuery(implicit params: Map[String,Seq[String]]) =
      BSONDocument(
          "$or" -> elemsToDisplay.map(elem => BSONDocument(elem -> filt)))

    def projectionQuery(implicit params: Map[String,Seq[String]]) =
      BSONDocument.apply(												//projection
              for{elem <- elemsToDisplay} yield {
            	  elem -> BSONBoolean(true)
              })
      
    def objsWithQuery(implicit params: Map[String,Seq[String]]) =
      obj.find(filterQuery, projectionQuery)
    
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
	def actualPageResult(iTotalRecords: Int)(implicit params: Map[String,Seq[String]]): Future[List[Seq[String]]] =
	  actualPage(iTotalRecords).cursor[Seq[String]](SeqObjReader(elemsToDisplay), defaultContext).collect[List]()
	
	import reactivemongo.core.commands._
	def iTotalRecordsResult =
	  singleton.count
	  
	def iTotalDisplayRecordsResult(implicit params: Map[String,Seq[String]]) =
	  singleton.db.command(Count(singleton.collectionName, Some(filterQuery), None))

  def table = Action.async { implicit request =>
    implicit val params = request.queryString

    import Json._
    import tp_utils.Jsoner._

    for {
      iTotalRecords <- iTotalRecordsResult
      iTotalDisplayRecords <- iTotalDisplayRecordsResult
      ap <- actualPageResult(iTotalRecords)
    } yield {
      pagerLog(s"table result -> 	${iTotalRecords}	${iTotalDisplayRecords}		${ap}")
      Ok(
        JsObject(
          Seq(
            "sEcho" -> toJsVal(sEcho),
            "iTotalRecords" -> toJsVal(iTotalRecords),
            "iTotalDisplayRecords" -> toJsVal(iTotalDisplayRecords),
            "aaData" ->
              JsArray(
                ap.map(elem =>
                  JsObject(
                    elemsToDisplay.zipWithIndex.map(e =>
                      e._2.toString -> toJson(elem(e._2))))).toSeq))))
    }
  }
}
