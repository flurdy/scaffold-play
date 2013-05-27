import play.api._
import play.api.mvc._
import play.api.mvc.Results._


object Global extends GlobalSettings {

  override def onStart(app: Application) {
    Logger.info("Application has started")
  }

  override def onStop(app: Application) {
    Logger.info("Application shutdown...")
  }

  override def onError(request: RequestHeader, exception: Throwable) = {
    InternalServerError(
      views.html.error.applicationError(exception)
    )
  }

  override def onHandlerNotFound(request: RequestHeader): Result = {
    NotFound(
      views.html.error.notFound(request.path)
    )
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    BadRequest("Bad Request: " + error)
  }

}