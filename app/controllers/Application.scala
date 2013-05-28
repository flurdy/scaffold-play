package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index(
        RegistrationAuthentication.simpleRegistrationForm,
        RegistrationAuthentication.loginForm))
  }

}

object RegistrationAuthentication extends Controller {

  val applicationUserMapping = mapping(
  "username" -> nonEmptyText.verifying(
        "Username is already taken. Please choose another",
        !ApplicationUser.isUsernameAlreadyRegistered(_)),
  "email" -> optional(email),
  "fullname" -> optional(text))(ApplicationUser.apply)(ApplicationUser.unapply)

  val passwordMapping = tuple (
      "password" -> nonEmptyText,
      "confirmPassword" -> nonEmptyText
    ) verifying( "Passwords did not match. Please enter passwords again", fields => fields match {
        case (password,confirmPassword) => password == confirmPassword
    })

	val userCredentialsMapping = mapping(
	  "applicationUser" -> applicationUserMapping,
    "passwords" -> passwordMapping
	  )(
	    (user,passwords) => UserCredentials( user, Some(passwords._1))
	  )(
	    (userCredentials: UserCredentials) => Some(userCredentials.user,("",""))
	  )

	val loginMapping = tuple(
	  "username" -> nonEmptyText,
	  "password" -> nonEmptyText
	  ) verifying("Invalid authentication", fields => fields match {
	    case (username,password) => UserCredentials.authenticate(username,password).isDefined
  	})


	val simpleRegistrationForm = Form( applicationUserMapping )
	val fullRegistrationForm = Form( userCredentialsMapping )
	val loginForm = Form( loginMapping )



  def startRegistrationProcess() = Action { implicit request =>
    Logger.info("startRegistrationProcess")
    simpleRegistrationForm.bindFromRequest.fold (
      formWithErrors => {
        Logger.info("form bad")
        Ok(views.html.fullregistration(formWithErrors,fullRegistrationForm))
      },
      maybeValue => {
        Logger.info("form ok")
        val prefilled = fullRegistrationForm.fill{
          new UserCredentials(maybeValue)
        }
        Ok(views.html.fullregistration(simpleRegistrationForm,prefilled))
      }
    )
  }

  def finishRegistrationProcess() = Action { implicit request =>
    Logger.info("finishRegistrationProcess")
    fullRegistrationForm.bindFromRequest.fold (
      formWithErrors => {
        Logger.info("form bad")
        BadRequest(views.html.fullregistration(simpleRegistrationForm,formWithErrors))
      },
      maybeValue => {
        Logger.info("form ok")
        //
        // Persist registration here
        //
        Ok(views.html.index(simpleRegistrationForm,loginForm))
      }
    )
  }

  def loginProcess() = Action { implicit request =>
    Logger.info("loginProcess")
    loginForm.bindFromRequest.fold(
      formWithErrors => {
        Logger.info("form bad")
        BadRequest(views.html.index(simpleRegistrationForm,formWithErrors))
      },
      loginValues => {
        Logger.info("form ok")
        //
        // Add login to session here
        //
        Ok(views.html.index(simpleRegistrationForm,loginForm))
      }
    )
  }

}