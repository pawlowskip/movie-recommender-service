package service

import model.Movie

import scala.concurrent.Future

/**
  * Created by pp on 5/5/16.
  */
trait MovieRecommenderApi {
  def getMovies(): Future[Seq[Movie]] = ???
  def getRecommendations(): Future[Seq[Movie]] = ???
  def rateMovie(movie: Movie): Future[Unit] = ???
}

