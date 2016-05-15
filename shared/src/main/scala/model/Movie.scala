package model

import search.core.SearchCriteria
import search.core.SearchCriteria.{Criteria, Field}

/**
  * Created by pp on 4/26/16.
  */

case class Poster(url: String)

object Poster{
  case class PosterUrl(criteria: SearchCriteria[String]) extends Field[String, Poster](_.url)
}

case class Movie(id: Long,
                 title: String,
                 year: Int,
                 poster: Poster,
                 averageRating: Double,
                 myRating: Option[Int],
                 viewers: Int,
                 description: String)

object Movie {
  
  case class MovieId(criteria: SearchCriteria[Long]) extends Field[Long, Movie](_.id)

  case class MovieTitle(criteria: SearchCriteria[String]) extends Field[String, Movie](_.title)

  case class MovieYear(criteria: SearchCriteria[Int]) extends Field[Int, Movie](_.year)

  case class MoviePoster(criteria: SearchCriteria[Poster]) extends Field[Poster, Movie](_.poster)

  case class MovieAverageRating(criteria: SearchCriteria[Double]) extends Field[Double, Movie](_.averageRating)

  case class MovieMyRating(criteria: SearchCriteria[Option[Int]]) extends Field[Option[Int], Movie](_.myRating)

  case class MovieViewers(criteria: SearchCriteria[Int]) extends Field[Int, Movie](_.viewers)

  case class MovieDescription(criteria: SearchCriteria[String]) extends Field[String, Movie](_.description)

}
