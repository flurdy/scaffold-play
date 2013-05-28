package models

import play.api._


case class ApplicationUser(username: String,
                          email: Option[String], fullname: Option[String] )

case class UserCredentials(user: ApplicationUser, password: Option[String]){
	def this(user: ApplicationUser) = this(user,None)
	def register = UserCredentials.register(this)
}

object UserCredentials {

	def authenticate(username: String, password: String) = {
		//
		// TODO Validate credentials with authentication service
		//
		None
	}

	def register(UserCredentials: UserCredentials) {
		// TODO persist
	}

}


object ApplicationUser {

	def isUsernameAlreadyRegistered(username: String): Boolean = findByUsername(username).isDefined

	private def findByUsername(username: String): Option[ApplicationUser] = {
		// TODO find application user in persistance store
		None
	}

}
