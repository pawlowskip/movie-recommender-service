package api

import model.{Movie, MovieCriteria, Rating}

import scala.concurrent.Future

/**
  * Created by pp on 5/5/16.
  */
trait MovieRecommenderApi {
  def getMyRatings: Future[Option[Seq[Rating]]]
  def getRecommendations: Future[Option[Seq[Rating]]]
  def rateMovie(rating: Rating): Future[Unit]
  def updateRating(rating: Rating): Future[Unit]
  def queryMovies(criteria: MovieCriteria): Future[Seq[Movie]]
}

