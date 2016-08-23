package api

import model.{Movie, MovieCriteria, Rating}

import scala.concurrent.Future

/**
  * Created by pp on 5/5/16.
  */
trait MovieRecommenderApi {
  def getMyRatings(userId: Long): Future[Option[Seq[Rating]]]
  def getRecommendations(userId: Long): Future[Option[Seq[Rating]]]
  def rateMovie(userId: Long, rating: Rating): Future[Unit]
  def updateRating(userId: Long, rating: Rating): Future[Unit]
  def queryMovies(userId: Long, criteria: MovieCriteria): Future[Seq[Movie]]
}

