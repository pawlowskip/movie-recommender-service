package model

/**
  * Created by pp on 8/23/16.
  */



case class BetweenInl[T](from: Option[T], to: Option[T]) {
  def isBetween(x: T)(implicit ord: Ordering[T]): Boolean = {
    (from, to) match {
      case (Some(f), Some(t)) => ord.gteq(x, f) && ord.lteq(x, t)
      case (Some(f), None) => ord.gteq(x, f)
      case (None, Some(t)) => ord.lteq(x, t)
      case (None, None) => false
    }
  }
}

case class BetweenExl[T](from: Option[T], to: Option[T]) {
  def isBetween(x: T)(implicit ord: Ordering[T]): Boolean = {
    (from, to) match {
      case (Some(f), Some(t)) => ord.gt(x, f) && ord.lt(x, t)
      case (Some(f), None) => ord.gt(x, f)
      case (None, Some(t)) => ord.lt(x, t)
      case (None, None) => false
    }
  }
}

sealed trait SortOrder
object Ascending extends SortOrder
object Descending extends SortOrder

sealed trait SortBy
case class SortByYear(order: SortOrder) extends SortBy
case class SortByAverageRating(order: SortOrder) extends SortBy
case class SortByViewers(order: SortOrder) extends SortBy

case class MovieCriteria(lastId: Long,
                         limit: Int,
                         titleContains: Option[String],
                         yearBetween: Option[BetweenInl[Int]],
                         averageRating: Option[BetweenInl[Double]],
                         viewers: Option[BetweenInl[Int]],
                         haveSeen: Option[Boolean],
                         sortBy: Option[SortBy])
