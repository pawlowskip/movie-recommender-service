package dao

import api.MovieRecommenderApi
import dao.RecommenderServiceDao.RecommenderServiceDaoException
import model.{BetweenInl, Movie, MovieCriteria, Rating}
import play.modules.reactivemongo.ReactiveMongoApi
import play.modules.reactivemongo.json.collection._
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.WriteResult
import reactivemongo.bson.{BSONDocument, BSONElement, BSONValue, BSONWriter}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
/**
  * Created by pp on 8/23/16.
  */
class RecommenderServiceDao(val movies: BSONCollection,
                            val users: BSONCollection) extends MovieRecommenderApi {

  import model.BsonHandlers._

  override def getMyRatings(userId: Long): Future[Option[Seq[Rating]]] =
    users.find(
      BSONDocument("id" -> userId),
      BSONDocument("myMovies" -> 1)
    ).one[List[Rating]]

  override def queryMovies(userId: Long, criteria: MovieCriteria): Future[Seq[Movie]] = criteria match {
    case MovieCriteria(page, limit, titleContains, yearBetween, averageRating, viewers, haveSeen, sortBy) =>
      val and = ListBuffer[BSONElement]()
      titleContains.foreach(title =>
        and += "$text" -> BSONDocument("$search" -> title)
      )
      yearBetween.foreach(between =>
       and ++= betweenToQuery("year", between)
      )
      averageRating.foreach(between =>
        and ++= betweenToQuery("averageRating", between)
      )
      viewers.foreach(between =>
        and ++= betweenToQuery("viewers", between)
      )
      val querySelector = and.toList

      ???
  }

  override def rateMovie(userId: Long, rating: Rating): Future[Unit] = {
    val result = users.update(
      BSONDocument("id" -> userId),
      BSONDocument("$addToSet" -> BSONDocument("myMovies" -> rating))
    )
    flatMapWriteResult(result)
  }

  override def updateRating(userId: Long, rating: Rating): Future[Unit] = {
    val result = users.update(
      BSONDocument("id" -> userId, "myMovies.$" -> rating.movieId),
      BSONDocument("$set" -> BSONDocument("myMovies.$.rating" -> rating.rate))
    )
    flatMapWriteResult(result)
  }

  override def getRecommendations(userId: Long): Future[Option[Seq[Rating]]] =
    users.find(
      BSONDocument("id" -> userId),
      BSONDocument("recommendations" -> 1)
    ).one[List[Rating]]

  private def flatMapWriteResult(writeRes: Future[WriteResult]): Future[Unit] =
    writeRes.flatMap{
      case updateWriteResult if updateWriteResult.ok => Future.successful(Unit)
      case updateWriteResult =>
        updateWriteResult.errmsg match {
          case Some(msg) => Future.failed(new RecommenderServiceDaoException(msg))
          case None => Future.failed(new RecommenderServiceDaoException("Error during updateRating!"))
        }
    }

  private def betweenToQuery[T](fieldName: String, between: BetweenInl[T])
                               (implicit writer: BSONWriter[T, BSONValue]): Seq[BSONElement] = between match {
    case BetweenInl(None, None) => Seq()
    case BetweenInl(Some(from), None) => Seq(fieldName -> BSONDocument("$gte" -> from))
    case BetweenInl(None, Some(to)) => Seq(fieldName -> BSONDocument("$lte" -> to))
    case BetweenInl(Some(from), Some(to)) =>
      Seq(
        fieldName-> BSONDocument("$gte" -> from),
        fieldName -> BSONDocument("$lte" -> to)
      )
  }
}

object RecommenderServiceDao {
  class RecommenderServiceDaoException(msg: String) extends Exception(msg)
}
