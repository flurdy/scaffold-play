package models

import play.api._


case class ApplicationUser(username: String,
                          email: Option[String], fullname: Option[String] )

case class UserCredentials(user: ApplicationUser, password: Option[String]){
	def this(user: ApplicationUser) = this(user,None)
}

object UserCredentials {

	def authenticate(username: String, password: String) = {
		//
		// Validate credentials with authentication service
		//
		None
	}

}

