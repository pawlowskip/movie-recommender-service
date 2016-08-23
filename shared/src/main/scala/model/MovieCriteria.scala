package model

/**
  * Created by pp on 8/23/16.
  */



case class BetweenInl[T](from: Option[T], to: Option[T])
case class BetweenExl[T](from: Option[T], to: Option[T])

sealed trait SortOrder
object Ascending extends SortOrder
object Descending extends SortOrder

sealed trait SortBy
case class SortByYear(order: SortOrder) extends SortBy
case class SortByAverageRating(order: SortOrder) extends SortBy
case class SortByViewers(order: SortOrder) extends SortBy

case class MovieCriteria(page: Int,
                         limit: Int,
                         titleContains: Option[String],
                         yearBetween: Option[BetweenInl[Int]],
                         averageRating: Option[BetweenInl[Double]],
                         viewers: Option[BetweenInl[Int]],
                         haveSeen: Option[Boolean],
                         sortBy: Option[SortBy])
