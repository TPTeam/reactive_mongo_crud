package controllers.helper

import play.api.mvc._
import play.api.libs.concurrent._
import models.persistence._
import play.api.libs.json._
import tpUtils.Tryer._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import Json._
import play.api.data._
import models._
import controllers.helpers.tpHelpers._
import play.core._
import scala.concurrent._
import reactivemongo.bson._

trait CRUDer[C <: ModelObj] extends SingletonDefiner[C] with Router.Routes {
  self: Controller =>

  def form: Form[C]

  def formTemplate(formb: Form[C])(implicit request: RequestHeader): play.twirl.api.Html

  def viewCreateForm(implicit request: RequestHeader): (Form[C] => Result) =
  {formb: Form[C] => Ok(formTemplate(formb))}
  def viewEditForm(implicit request: RequestHeader): (Form[C] => Result) =
  {formb: Form[C] => Ok(formTemplate(formb))}
  def viewBadForm(implicit request: RequestHeader): (Form[C] => Result) =
  {formb: Form[C] => BadRequest(formTemplate(formb))}

  def createForm: Form[C] = form
  def editForm(elem: C): Form[C] = form.fill(elem)


  def create = Action  {
    implicit request =>
      viewCreateForm.apply(createForm)
  }

  def edit(id: String): EssentialAction = Action.async {
    implicit request =>
      BSONObjectID.parse(id).toOption match {
        case Some(oid) =>
          for {
            o <- obj.findOneById(oid)
          } yield (o) match {
            case Some(elem) =>
              viewEditForm.apply(editForm(elem))
            case _ =>
              Ok("Cannot edit undefined object")
          }
        case _ => throw new Error(s"[ReactiveMongoPlugin] : invalid object ID for CRUDer.edit ${id}")
      }
  }

  private val _formObjectID = "id"

  case class FormBinder(form: () => Form[C])(implicit request: Request[AnyContent]) {
    def bindFromRequest: Future[Result] = {
      Future{
        getObjectToRemove(_formObjectID) match {
          case Some(toRem) =>
            deletion(toRem)
          case None => val _form = form.apply
            _form.bindFromRequest.fold(
              badForm =>
                _form.bind(
                  badForm.data ++
                    badForm.errors.seq.map(err => (err.key -> ""))
                ).fold(
                    definitiveBad => viewBadForm.apply(badForm),
                    elem => success/*(elem)*/),
              elem => success/*(elem)*/
            )}
      }
    }
  }

  def submit =
    Action.async{
      implicit request => {
        val data =
          request.body.asFormUrlEncoded match {
            case Some(content) =>
              content
            case _ =>
              Map()
          }

        FormBinder(() => form).bindFromRequest
      }
    }

  def deletion(elemId: BSONObjectID)(implicit request: Request[AnyContent]) = {
    obj.delete(elemId)
    Ok("Removed")
  }

  def success/*(elem: C)*/(implicit request: Request[AnyContent]) =
    Ok("Ok")

  /*
   * Routable element
   */

  implicit lazy val paths =
    CRUDerPaths(
      prefix+"/edit",
      prefix+"/new",
      prefix+"/submit"
    )

  private val promiseOfPath: scala.concurrent.Promise[String] =
    scala.concurrent.Promise[String]
  private val futurePath = promiseOfPath.future

  def setPrefix(prefix: String) = {
    promiseOfPath.success(prefix)
  }
  def prefix = {
    tryo{futurePath.value.get.get}.getOrElse("")
  }

  def documentation = Nil

  def routes =
    new scala.runtime.AbstractPartialFunction[RequestHeader, Handler] {
      override def applyOrElse[A <: RequestHeader, B >: Handler](rh: A, default: A => B) = {
        val toCheck =
          if (rh.path.startsWith(prefix))
          //to be REIMPLEMENTED
            prefix + "/" + tryo{rh.path.replaceFirst(prefix, "").split("/").tail.head}.getOrElse("")
          else
            rh.path

        (rh.method, toCheck) match {
          case ("GET",paths.create) => create
          case ("GET", paths.edit) => edit(rh.path.split("/").last)
          case ("POST", paths.submit) => submit
          case _ => default(rh)
        }
      }
      def isDefinedAt(rh: RequestHeader) =
        (rh.method, rh.path.drop(1)) match {
          case ("GET", "create") => true
          case ("GET", "edit") => true
          case ("POST", "submit") => true
          case _ => false
        }
    }

}

case class CRUDerPaths(
                        edit: String,
                        create: String,
                        submit: String
                        ) {
  override def toString = 
    Json.obj(
      "create" -> create,
      "edit" -> (edit+"/"),
      "submit" -> submit
    ).toString

  def getJson =
    Json.obj(
      "create" -> create,
      "edit" -> edit,
      "submit" -> submit
    )
}