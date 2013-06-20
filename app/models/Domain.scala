package models

import play.api._
import org.mindrot.jbcrypt.BCrypt


case class ApplicationUser(userId: Option[String], username: String,
                          email: Option[String], fullname: Option[String] ) {
	def this(username: String, email: Option[String],
				 fullname: Option[String]) = this(None,username,email,fullname)
}

case class UserCredentials(user: ApplicationUser, password: Option[String], permission: Option[Permission]){
	def this(user: ApplicationUser) = this( user, None, None)
	def register = UserCredentials.register(this)
}

sealed trait Permission
case object Administrator extends Permission
case object NormalUser extends Permission
case object Anyone extends Permission

object UserCredentials {

	def authenticate(username: String, enteredPassword: String) = {
		ApplicationUser.findByUsername(username) flatMap { applicationUser =>
			findCredentials(applicationUser) flatMap { hashedPassword =>
				if(BCrypt.checkpw(enteredPassword, hashedPassword)){
					Logger.info(s"Authenticated $username")
					findByApplicationUser(applicationUser)
				} else None
			}
		}
	}


	def findById(userId: String): Option[UserCredentials] = ApplicationUser.findById(userId) flatMap ( findByApplicationUser(_) )

	def findByApplicationUser(applicationUser: ApplicationUser): Option[UserCredentials] = {
		applicationUser.userId flatMap { userId =>
//			 map { permission =>
			Some(UserCredentials(applicationUser, None, Some(NormalUser)))
//			}
		}
	}

	private def findCredentials(applicationUser: ApplicationUser): Option[String] = {
		// TODO Lookup credentials in persistance store
		None
	}

	def register(userCredentials: UserCredentials): String = {
		// TODO store registration in persistance store

		userCredentials.password map { password =>
			val encryptedPassword = encrypt(password)
		}
		"123"
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


	def findById(userId: String): Option[ApplicationUser] = {
		val username = ""
		val email = ""
		val fullname = ""
		Some(ApplicationUser(Some(userId),username,Some(email),Some(fullname)))
	}

}
