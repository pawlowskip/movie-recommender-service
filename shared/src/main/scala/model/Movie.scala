package model

/**
  * Created by pp on 4/26/16.
  */

case class Poster(url: String)
case class Movie(id: Long,
                 title: String,
                 year: Int,
                 poster: Poster,
                 averageRating: Double,
                 myRating: Option[Int],
                 viewers: Int,
                 description: String)
