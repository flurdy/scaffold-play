package models

import play.api._
import org.mindrot.jbcrypt.BCrypt


case class ApplicationUser(userId: Option[String], username: String,
                          email: Option[String], fullname: Option[String] ) {
	def this(username: String, email: Option[String],
				 fullname: Option[String]) = this(None,username,email,fullname)
}

case class UserCredentials(user: ApplicationUser, password: Option[String]){
	def this(user: ApplicationUser) = this(user,None)
	def register = UserCredentials.register(this)
}

object UserCredentials {

	def authenticate(username: String, enteredPassword: String) = {
		ApplicationUser.findByUsername(username) map { applicationUser =>
			applicationUser.userId map { userId =>
				findCredentials(userId) map { hashedPassword =>
					if(BCrypt.checkpw(enteredPassword,hashedPassword)){
						Logger.info(s"Authenticated $username")
						Some(applicationUser)
					} else None
				}
			}
		}
	}

	private def findCredentials(userId: String): Option[String] = {
		// TODO Lookup credentials in persistance store
		None
	}

	def register(userCredentials: UserCredentials) {
		// TODO store registration in persistance store

		userCredentials.password map { password =>
			val encryptedPassword = encrypt(password)
		}
	}

	private def encrypt(password: String): String = {
		BCrypt.hashpw(password,BCrypt.gensalt())
	}

}


object ApplicationUser {

	def isUsernameAlreadyRegistered(username: String): Boolean = findByUsername(username).isDefined

	def findByUsername(username: String): Option[ApplicationUser] = {
		// TODO find application user in persistance store
		None
	}

}
