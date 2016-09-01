package controllers

import auth.AuthenticationDao
import bodyparser.CustomBodyParsers._
import dao.MovieRecommenderDao
import jp.t2v.lab.play2.auth.AuthElement
import model.{MovieCriteria, Rating, User}
import play.api.Logger
import play.api.mvc._
import upickle.default._

import scala.util.control.NonFatal

class RecommenderController(val movieRecommenderDao: MovieRecommenderDao, val accountsDao: AuthenticationDao)
  extends Controller with AuthElement with AuthConfigImpl {

  def index = Action {
    Logger.info(s"Serving index page.")
    Ok(views.html.index(""))
  }

  def getMyRatings = AsyncStack(AuthorityKey -> User.Role.normal){ implicit request =>
    val userId = loggedIn.id
    Logger.info(s"Upcoming request getMyRatings/$userId.")
    movieRecommenderDao.getMyRatings(userId)
      .map(toUpickleJSON)
      .recover{ case err =>
        Logger.error(s"getMyRatings/$userId - Error during executing request.", err)
        Results.InternalServerError(s"Error during executing query: $err")
      }
  }


  def getRecommendations = AsyncStack(AuthorityKey -> User.Role.normal){ implicit request =>
    val userId = loggedIn.id
    Logger.info(s"Upcoming request getRecommendations/$userId.")
    movieRecommenderDao.getRecommendations(userId)
      .map(toUpickleJSON)
      .recover{ case err =>
        Logger.error(s"getRecommendations/$userId - Error during executing request.", err)
        Results.InternalServerError(s"Error during executing query: $err")
      }
  }

  def rateMovie = AsyncStack(upickleJson, AuthorityKey -> User.Role.normal){
    val userId = loggedIn.id
    implicit request: Request[Rating] =>
      Logger.info(s"Upcoming request rateMovie/$userId.")
      movieRecommenderDao.rateMovie(userId, request.body)
        .map(unit => Results.Created(s"Created rating."))
        .recover{ case err =>
          Logger.error(s"rateMovie/$userId - Error during executing request.", err)
          Results.InternalServerError(s"Error during executing query: $err")
        }
  }

  def updateRating = AsyncStack(upickleJson, AuthorityKey -> User.Role.normal){
    val userId = loggedIn.id
    implicit request: Request[Rating] =>
      Logger.info(s"Upcoming request updateRating/$userId.")
      movieRecommenderDao.updateRating(userId, request.body)
        .map(unit => Results.Ok(s"Updated rating."))
        .recover{ case err =>
          Logger.error(s"updateRating/$userId - Error during executing request.", err)
          Results.InternalServerError(s"Error during executing query: $err")
        }
  }

  def queryMovies = AsyncStack(upickleJson, AuthorityKey -> User.Role.normal){
    val userId = loggedIn.id
    implicit request: Request[MovieCriteria] =>
      Logger.info(s"Upcoming request queryMovies/$userId.")
      movieRecommenderDao.queryMovies(userId, request.body)
        .map(toUpickleJSON)
        .recover{ case err =>
          Logger.error(s"queryMovies/$userId - Error during executing request.", err)
          Results.InternalServerError(s"Error during executing request: $err")
        }
  }


  private def toUpickleJSON[T](t: T): Result = {
    try {
      Ok(write(t)).as(JSON)
    } catch {
      case NonFatal(e) => Results.InternalServerError(s"Error during parsing response: $e")
    }
  }

}
