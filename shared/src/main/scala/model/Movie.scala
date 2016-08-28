package model

/**
  * Created by pp on 4/26/16.
  */

case class Rating(movieId: Long, rate: Double)

case class Movie(id: Long,
                 title: String,
                 year: Int,
                 posterUrl: String,
                 averageRating: Double,
                 viewers: Int,
                 description: String)

