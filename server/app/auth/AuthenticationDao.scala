package auth
import model.User

import scala.concurrent.Future
/**
  * Created by pp on 8/31/16.
  */
trait AuthenticationDao {
  def findUserByLogin(login: String): Future[Option[User]]
  def checkUsernameAvailability(login: String): Future[Option[Long]]
  def createAccount(user: User): Future[Unit]
}
