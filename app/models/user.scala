package models

import play.api._
import org.mindrot.jbcrypt.BCrypt


case class ApplicationUser(userId: Option[String], username: String,
                          email: Option[String], fullname: Option[String] ) {
	def this(username: String, email: Option[String],
				 fullname: Option[String]) = this(None,username,email,fullname)
	def updateEmail = ApplicationUser.updateEmail(this)
	def updateFullname = ApplicationUser.updateFullname(this)
}

case class UserCredentials(user: ApplicationUser,
					password: Option[String], permission: Option[Permission]){
	def this(user: ApplicationUser) = this( user, None, None)
	def register = UserCredentials.register(this)
	def changePassword = UserCredentials.changePassword(this)
	def delete = UserCredentials.deleteCredentials(this)
}

sealed trait Permission
case object Administrator extends Permission
case object NormalUser extends Permission
case object Anyone extends Permission

object Permission {

  def valueOf(value: String): Permission = value match {
    case "Administrator" => Administrator
    case "NormalUser"    => NormalUser
    case "Anyone"    => Anyone
    case _ => throw new IllegalArgumentException()
  }

}


object UserCredentials {

	def authenticate(username: String, enteredPassword: String) = {
		ApplicationUser.findByUsername(username) match {
      case None => {
        Logger.debug(s"No user found with username $username")
        None
      }
      case Some(applicationUser) => {
        findCredentials(applicationUser) match {
          case None => {
            Logger.warn(s"No credentials found for application user ${applicationUser.userId.get}")
            None
          }
          case Some(hashedPassword) => {
            if(BCrypt.checkpw(enteredPassword, hashedPassword)){
              findByApplicationUser(applicationUser)
            } else {
              Logger.debug(s"Password mismatch for application user ${applicationUser.userId.get}")
              None
            }
          }
        }
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

	def changePassword(userCredentials: UserCredentials): UserCredentials = {
		val username = userCredentials.user.username
		Logger.info(s"Changing password for $username")

			for{
				userId <- userCredentials.user.userId
				password <- userCredentials.password
			} yield {
				// TODO
				encrypt(password)
			}

		userCredentials
	}

	def deleteCredentials(userCredentials: UserCredentials) {
		Logger.info(s"Deleting credentials for ${userCredentials.user.username}")
		ApplicationUser.deleteApplicationUser(userCredentials.user)

		userCredentials.user.userId.map { userId =>

			// TODO

		}

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



	def updateFullname(applicationUser: ApplicationUser) = {
		applicationUser.userId map { userId =>
			applicationUser.fullname match {
				case Some(fullname) => {
					// TODO update
				}
				case None => {
					// TODO remove
				}
			}
		}
		applicationUser
	}

	def updateEmail(applicationUser: ApplicationUser) = {
		applicationUser.userId map { userId =>
			applicationUser.email match {
				case Some(email) => {
					// TODO update
				}
				case None => {
					// TODO remove
				}
			}
		}
		applicationUser
	}

	def deleteApplicationUser(applicationUser: ApplicationUser) {
		Logger.info(s"Deleting user for ${applicationUser.username}")
		applicationUser.userId.map { userId =>

			// TODO cascade any app domain deletions

			// TODO delete
		}
	}

}




