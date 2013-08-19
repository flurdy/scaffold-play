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



object RegistrationAuthentication extends Controller with LoginLogout with CustomAuthConfig with NotAuthenticatedUser {

  val applicationUserMapping = mapping(
  "username" -> nonEmptyText.verifying(
        "Username is already taken. Please choose another",
        !ApplicationUser.isUsernameAlreadyRegistered(_)),
  "email" -> optional(email),
  "fullname" -> optional(text))(
    (username,email,fullname) => new ApplicationUser(username,email,fullname)
  )(
    (applicationUser:ApplicationUser) => Some((applicationUser.username,applicationUser.email,applicationUser.fullname))
  )

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
	    (user,passwords) => UserCredentials( user, Some(passwords._1), None)
	  )(
	    (userCredentials: UserCredentials) => Some(userCredentials.user,("",""))
	  )

	val loginMapping = tuple(
	  "username" -> nonEmptyText,
	  "password" -> nonEmptyText
	  ) verifying("Authentication failed. Please verify your credentials or register", fields => fields match {
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
        Ok(views.html.register(formWithErrors,fullRegistrationForm))
      },
      maybeValue => {
        Logger.info("form ok")
        val prefilled = fullRegistrationForm.fill{
          new UserCredentials(maybeValue)
        }
        Ok(views.html.register(simpleRegistrationForm,prefilled))
      }
    )
  }

  def finishRegistrationProcess() = Action { implicit request =>
    Logger.info("finishRegistrationProcess")
    fullRegistrationForm.bindFromRequest.fold (
      formWithErrors => {
        Logger.info("registration form bad")
        BadRequest(views.html.register(simpleRegistrationForm,formWithErrors))
      },
      registrationValues => {
        val userId = registrationValues.register
        gotoLoginSucceeded(userId).flashing("successMessage"->"Registration succeeded")
      }
    )
  }

  def loginProcess() = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => {
        Logger.info("login form bad")
        BadRequest(views.html.login(formWithErrors))
      },
      loginValues => {
        ApplicationUser.findByUsername(loginValues._1) flatMap { _.userId } match {
          case Some(userId) => gotoLoginSucceeded(userId)
          case None => BadRequest(views.html.index(
                simpleRegistrationForm,loginForm.fill((loginValues._1,""))))
        }
      }
    )
  }

  def showRegistration = Action {
    Ok(views.html.register(simpleRegistrationForm,fullRegistrationForm))
  }

  def redirectToRegistration = Action {
    Redirect(routes.RegistrationAuthentication.showRegistration)
  }

  def showLogin = Action {
    Ok(views.html.login(loginForm))
  }

  def logout = Action { implicit request =>
    gotoLogoutSucceeded
  }

}




trait AuthenticatedUser extends AuthElement {
    self: Controller with AuthConfig =>

  implicit def currentApplicationUser[A](implicit request: RequestWithAttributes[A]): Option[ApplicationUser] = {
    loggedIn(request) match {
      case userCredentials:UserCredentials => Some(userCredentials.user)
      case _ => {
        Logger.info("no credentials found")
        None
      }
    }
  }

}

trait OptionalAuthenticatedUser extends OptionalAuthElement {
    self: Controller with AuthConfig =>

  implicit def currentApplicationUser[A](implicit request: RequestWithAttributes[A]): Option[ApplicationUser] = {
    loggedIn(request) match {
      case Some(userCredentials:UserCredentials) => Some(userCredentials.user)
      case _ => None
    }
  }

}

trait NotAuthenticatedUser extends OptionalAuthElement {
    self: Controller with AuthConfig =>

  implicit def currentApplicationUser[A](implicit request: Request[A]): Option[ApplicationUser] = None

}


trait CustomAuthConfig extends AuthConfig {

  type Id = String

  type User = UserCredentials

  type Authority = Permission


  val idTag: ClassTag[Id] = classTag[Id]

  val sessionTimeoutInSeconds : Int = 3600


  def resolveUser(id: Id): Option[User] = UserCredentials.findById(id)

  def loginSucceeded(request: RequestHeader): Result = {
    // Logger.info("Login succeeded")
    // val uri = request.session.get("access_uri").getOrElse(routes.Application.index.url.toString)
    // Redirect(uri).withSession(request.session - "access_uri")
    Redirect(routes.Application.index)//.withSession(request.session - "access_uri")
  }

  def logoutSucceeded(request: RequestHeader): Result = {
    // Logger.info("Logout succeeded")
    Redirect(routes.Application.index)
  }

  def authenticationFailed(request: RequestHeader): Result = {
    Logger.warn("Authentication failed")
    Redirect(routes.RegistrationAuthentication.showLogin) //.withSession("access_uri" -> request.uri)
  }

  def authorizationFailed(request: RequestHeader): Result = {
    Logger.warn("Authorization failed")
    Redirect(routes.RegistrationAuthentication.showLogin)
  }

  def authorize(user: User, authority: Authority): Boolean = {
    // Logger.info("Authorize lookup")
    (user.permission, authority) match {
      case (Some(Administrator), _) => true
      case (Some(NormalUser), NormalUser) => true
      case (_, Anyone) => true
      case _ => false
    }
  }

  // override lazy val cookieSecureOption: Boolean =
  //    play.api.Play.current.configuration.getBoolean("auth.cookie.secure").getOrElse(false)

  override lazy val idContainer: IdContainer[Id] = new CookieIdContainer[Id]

}




object Profile extends Controller with AuthenticatedUser with CustomAuthConfig {

 val applicationUserMapping = mapping(
  "email" -> optional(email),
  "fullname" -> optional(text))(
    (email,fullname) => new ApplicationUser("",email,fullname)
  )(
    (applicationUser:ApplicationUser) => Some((applicationUser.email,applicationUser.fullname))
  )

  val profileForm = Form( applicationUserMapping )


  def showProfile(profileId: String) = StackAction(AuthorityKey -> NormalUser) { implicit request =>

    if(currentApplicationUser.get.userId.get == profileId) {
      val profile = ApplicationUser.findById(profileId).get
      val prefilledForm = profileForm.fill(profile)
      Ok(views.html.profile.profile(profile,profileForm))

    } else {
      Logger.warn("show profile Unauthorized")
      Unauthorized
    }
  }

  def updateProfile(profileId: String) = StackAction(AuthorityKey -> NormalUser) { implicit request =>
    currentApplicationUser match {
      case Some(currentUser) => {
        if(currentUser.userId.get == profileId) {
          profileForm.bindFromRequest.fold (
            formWithErrors => {
              Logger.warn("Update profile form bad")
              Ok(views.html.profile.profile(currentApplicationUser.get,formWithErrors))
            },
            maybeValue => {

              ApplicationUser.findByUsername(currentUser.username) map { appUser =>

                appUser.copy(fullname = maybeValue.fullname).updateFullname.copy(email = maybeValue.email).updateEmail

              }

              Redirect(routes.Profile.showProfile(profileId))
            }
          )
        } else {
          Logger.warn("Update profile Unauthorized")
          Unauthorized
        }
      }
      case None => throw new IllegalStateException("No logged in user found")
    }
  }

  val changePasswordMapping = tuple (
    "username" -> nonEmptyText,
    "oldPassword" -> nonEmptyText,
    "newPassword" -> nonEmptyText,
    "confirmPassword" -> nonEmptyText
  ) verifying( "Passwords did not match. Please enter passwords again", fields => fields match {
      case (username,oldPassword,newPassword,confirmPassword) => newPassword == confirmPassword
  }) verifying("Authentication failed. Please verify your old password", fields => fields match {
    case (username,oldPassword,newPassword,confirmPassword) =>  UserCredentials.authenticate(username,oldPassword).isDefined
  })

  val passwordForm = Form( changePasswordMapping )


  def showChangePassword(profileId: String) = StackAction(AuthorityKey -> NormalUser) { implicit request =>

    Ok(views.html.profile.password(currentApplicationUser.get,passwordForm))
  }


  def changePassword(profileId: String) = StackAction(AuthorityKey -> NormalUser) { implicit request =>
    currentApplicationUser match {
      case Some(currentUser) => {
        if(currentUser.userId.get == profileId) {
          passwordForm.bindFromRequest.fold (
            formWithErrors => {
              Logger.warn("Change password form bad")
              Ok(views.html.profile.password(currentApplicationUser.get,formWithErrors))
            },
            maybeValue => {

              UserCredentials.findById(profileId) map { credentials =>
                credentials.copy(password = Some(maybeValue._3)).changePassword
              }

              Redirect(routes.RegistrationAuthentication.logout).flashing("messageSuccess" -> "Password changed. Please log in again")
            }
          )
        } else {
          Logger.warn("change password Unauthorized")
          Unauthorized
        }
      }
      case None => throw new IllegalStateException("No logged in user found")
    }
  }


  def showDeleteProfile(profileId: String) = StackAction(AuthorityKey -> NormalUser) { implicit request =>
    Ok(views.html.profile.delete(currentApplicationUser.get))
  }


  def deleteProfile(profileId: String) = StackAction(AuthorityKey -> NormalUser) { implicit request =>
    currentApplicationUser match {
      case Some(currentUser) => {
        if(currentUser.userId.get == profileId) {

          UserCredentials.findById(profileId) map { credentials =>
            credentials.delete
          }

          Redirect(routes.RegistrationAuthentication.logout)
        } else {

          Logger.warn("Delete profile Unauthorized")
          Unauthorized
        }
      }
      case None => throw new IllegalStateException("No logged in user found")
    }
  }

}


object OpenProfile extends Controller with OptionalAuthenticatedUser with CustomAuthConfig {


  def showResetPassword = StackAction { implicit request =>
    Ok(views.html.profile.reset(resetPasswordForm))
  }

  def showPasswordReset = StackAction { implicit request =>
    Ok(views.html.profile.passwordReset())
  }

  val resetPasswordForm = Form( tuple(
      "username" -> nonEmptyText,
      "email" -> email
  ) )

  def resetPassword = StackAction { implicit request =>
    currentApplicationUser match {
      case Some(currentUser) => {

          Redirect(routes.Application.index)
      }
      case None => {

          // TODO
          Logger.warn("Resetting password postponed until email is integrated")

          Redirect(routes.OpenProfile.showPasswordReset())
      }
    }
  }


}




