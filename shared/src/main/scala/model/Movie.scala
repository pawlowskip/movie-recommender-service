package model

import search.core.SearchCriteria
import search.core.SearchCriteria.FieldCriteria

/**
  * Created by pp on 4/26/16.
  */

case class Poster(url: String)

object Poster{
  case class Url(inputCriteria: SearchCriteria[String]) extends FieldCriteria[Poster] {
    override val criteria = SearchCriteria[Poster](m => inputCriteria.check(m.url))
  }

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
  val MovieID =

  case class MovieId(inputCriteria: SearchCriteria[Long]) extends FieldCriteria[Movie] {
    override val criteria = SearchCriteria[Movie](m => inputCriteria.check(m.id))
  }


  case class MovieTitle(inputCriteria: SearchCriteria[String])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.title)))

  case class MovieYear(inputCriteria: SearchCriteria[Int])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.year)))

  case class MoviePoster(inputCriteria: SearchCriteria[Poster])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.poster)))

  case class MovieAverageRating(inputCriteria: SearchCriteria[Double])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.averageRating)))

  case class MovieMyRating(inputCriteria: SearchCriteria[Option[Int]])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.myRating)))

  case class MovieViewers(inputCriteria: SearchCriteria[Int])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.viewers)))

  case class MovieDescription(inputCriteria: SearchCriteria[String])
    extends FieldCriteria[Movie](SearchCriteria(m => inputCriteria.check(m.description)))

}