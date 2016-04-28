package model

/**
  * Created by pp on 4/26/16.
  */

case class Poster(url: String)
case class Movie(id: Long, name: String, poster: Poster, rating: Double, viewers: Int, description: String)
