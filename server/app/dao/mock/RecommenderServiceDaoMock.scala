package dao.mock

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import api.MovieRecommenderApi
import dao.MovieRecommenderDao
import dao.RecommenderServiceDao.RecommenderServiceDaoException
import model._

import scala.collection.JavaConverters._
import scala.concurrent.Future

/**
  * Created by pp on 8/27/16.
  */
class RecommenderServiceDaoMock extends MovieRecommenderDao {
  val idGeneratorMovies = new AtomicInteger(1)
  val idGeneratorUsers = new AtomicInteger(1)
  val movies = new ConcurrentHashMap[Long, Movie].asScala
  val users = new ConcurrentHashMap[Long, User].asScala
  val usersRatings = new ConcurrentHashMap[Long, Vector[Rating]].asScala
  val usersRecommendations = new ConcurrentHashMap[Long, Vector[Rating]].asScala

  override def getMyRatings(userId: Long): Future[Option[Seq[Rating]]] =
    Future.successful(usersRatings.get(userId))

  private def filterOption[T](opt: Option[T], f: T => Boolean, none: Boolean = true): Boolean = opt match {
    case Some(t) => f(t)
    case None => none
  }

  override def queryMovies(userId: Long, criteria: MovieCriteria): Future[Seq[Movie]] = {
    def filterHaveSeen(haveSeen: Option[Boolean], movieId: Long): Boolean = haveSeen match {
      case Some(true) => usersRatings(userId).exists(_.movieId == movieId)
      case Some(false) => true
      case None => true
    }

    criteria match {
      case MovieCriteria(lastId, limit, titleContains, yearBetween, averageRatingBetween, viewersBetween, haveSeen,
                         sortBy) =>
        val serched = movies.filter { case (_, movie) =>
          movie.id > lastId &&
            filterOption(titleContains, title => movie.title.contains(title)) &&
            filterOption[BetweenInl[Int]](yearBetween, between => between.isBetween(movie.year)) &&
            filterOption[BetweenInl[Double]](averageRatingBetween, between => between.isBetween(movie.averageRating)) &&
            filterOption[BetweenInl[Int]](viewersBetween, between => between.isBetween(movie.viewers)) &&
            filterHaveSeen(haveSeen, movie.id)
        }
        val values = serched.values.toSeq
        val sorted = sortBy match {
          case None => values
          case Some(SortByViewers(Ascending)) => values.sortBy(_.viewers)
          case Some(SortByViewers(Descending)) => values.sortBy(_.viewers)(Ordering[Int].reverse)
          case Some(SortByAverageRating(Ascending)) => values.sortBy(_.averageRating)
          case Some(SortByAverageRating(Descending)) => values.sortBy(_.averageRating)(Ordering[Double].reverse)
          case Some(SortByYear(Ascending)) => values.sortBy(_.year)
          case Some(SortByYear(Descending)) => values.sortBy(_.year)(Ordering[Int].reverse)
        }
        Future.successful(sorted)
    }
  }


  override def rateMovie(userId: Long, rating: Rating): Future[Unit] = {
    if (!usersRatings.isDefinedAt(userId))
      Future.failed(new RecommenderServiceDaoException(s"There is no such user id=[$userId]"))
    else {
      if (usersRatings.get(userId).exists(_.exists(_.movieId == rating.movieId))) {
        Future.failed(new RecommenderServiceDaoException(s"Rating already exists for movie id=[${rating.movieId}]"))
      }
      usersRatings.update(rating.movieId, usersRatings.get(userId).get :+ rating)
      Future.successful(Unit)
    }
  }

  override def updateRating(userId: Long, rating: Rating): Future[Unit] = {
    if (!usersRatings.isDefinedAt(userId))
      Future.failed(new RecommenderServiceDaoException(s"There is no such user id=[$userId]"))
    else {
      if (usersRatings.get(userId).exists(_.exists(_.movieId == rating.movieId))) {
        Future.failed(new RecommenderServiceDaoException(s"There is no such rating for movie id=[${rating.movieId}]"))
      }
      usersRatings.update(rating.movieId, usersRatings.get(userId).get.filter(_.movieId != rating.movieId) :+ rating)
      Future.successful(Unit)
    }
  }

  override def getRecommendations(userId: Long): Future[Option[Seq[Rating]]] = {
    if (!usersRecommendations.isDefinedAt(userId))
      Future.failed(new RecommenderServiceDaoException(s"There is no such user id=[$userId]"))
    else {
      Future.successful(usersRecommendations.get(userId))
    }
  }
}
