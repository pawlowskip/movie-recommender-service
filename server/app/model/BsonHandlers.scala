package model
import reactivemongo.bson._
/**
  * Created by pp on 8/23/16.
  */
object BsonHandlers {
  implicit val movieHandler: BSONHandler[BSONDocument, Movie] = Macros.handler[Movie]
  implicit val ratingHandler: BSONHandler[BSONDocument, Rating] = Macros.handler[Rating]
}
