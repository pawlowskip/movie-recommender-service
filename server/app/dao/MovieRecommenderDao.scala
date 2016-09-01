package dao

import model.{Movie, MovieCriteria, Rating}

import scala.concurrent.Future

/**
  * Created by pp on 8/31/16.
  */
trait MovieRecommenderDao {
  def getMyRatings(id: Long): Future[Option[Seq[Rating]]]
  def getRecommendations(id: Long): Future[Option[Seq[Rating]]]
  def rateMovie(id: Long, rating: Rating): Future[Unit]
  def updateRating(id: Long, rating: Rating): Future[Unit]
  def queryMovies(id: Long, criteria: MovieCriteria): Future[Seq[Movie]]
}
