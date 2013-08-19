package controllers

import play.api._
import play.api.Play._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.data._
import play.api.data.Forms._
import models._
import jp.t2v.lab.play2.auth._
import reflect.classTag
import reflect.ClassTag
import jp.t2v.lab.play2.stackc.{RequestWithAttributes, RequestAttributeKey, StackableController}

object Application extends Controller with OptionalAuthenticatedUser with CustomAuthConfig {

  def index = StackAction { implicit request =>
    Ok(views.html.index(
        RegistrationAuthentication.simpleRegistrationForm,
        RegistrationAuthentication.loginForm))
  }

}




