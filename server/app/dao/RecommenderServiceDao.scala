package dao

import api.MovieRecommenderApi
import dao.RecommenderServiceDao.RecommenderServiceDaoException
import model._
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands.{UpdateWriteResult, WriteResult}
import reactivemongo.bson.{BSONDocument, BSONElement, BSONValue, BSONWriter}
import dao.MongoDocumentsNaming._
import play.modules.reactivemongo.{ReactiveMongoApi}
import reactivemongo.api.Cursor
import play.api.Logger

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
/**
  * Created by pp on 8/23/16.
  */
class RecommenderServiceDao(val reactiveMongoApi: ReactiveMongoApi)(implicit ec: ExecutionContext)
  extends MovieRecommenderDao {

  import model.BsonHandlers._

  // db from application.conf
  def database = reactiveMongoApi.database

  lazy val movies: Future[BSONCollection] = database.map(_.collection(Movies.collectionName))
  lazy val users: Future[BSONCollection] = database.map(_.collection(Users.collectionName))

  override def getMyRatings(userId: Long): Future[Option[Seq[Rating]]] =
    for {
      collection <- users
      result <- collection.find(
                  BSONDocument(Users.id -> userId),
                  BSONDocument(Users.myRatings -> 1)
                ).one[List[Rating]]
    } yield result

  override def queryMovies(userId: Long, criteria: MovieCriteria): Future[Seq[Movie]] = {
    def orderToQueryValue(order: SortOrder): Int = order match {
      case Ascending => 1
      case Descending => -1
    }
    /**
      * sorting by field id is because of pagination
      * @param sortOpt
      * @return
      */
    def sortQuery(sortOpt: Option[SortBy]): BSONDocument = sortOpt match {
      case Some(SortByYear(order)) =>
        BSONDocument(Movies.id -> 1, Movies.year -> orderToQueryValue(order))
      case Some(SortByAverageRating(order)) =>
        BSONDocument(Movies.id -> 1,Movies.averageRating -> orderToQueryValue(order))
      case Some(SortByViewers(order)) =>
        BSONDocument(Movies.id -> 1,Movies.viewers -> orderToQueryValue(order))
      case None =>
        BSONDocument(Movies.id -> 1)
    }

    def aggregationPipelineMyMovies(collection: BSONCollection,
                                    lastId: Long,
                                    limit: Int,
                                    querySelector: List[(String, BSONValue)],
                                    sort: BSONDocument): List[collection.PipelineOperator] = {

      import collection.BatchCommands.AggregationFramework.{
        Project, Match, Unwind, Lookup, Sort, Ascending, Descending, Limit
      }

      val fieldNameForLookup = "my_movies"
      List(
        Match(BSONDocument(Users.id -> userId)),
        Unwind("$" + Users.myRatings),
        Lookup(
          from = "movies",
          localField = Users.myRatings + "." + Ratings.movieId,
          foreignField = Movies.id,
          as = fieldNameForLookup),
        Unwind("$" + fieldNameForLookup),
        Project(
          BSONDocument(
            Movies.id -> ("$" + fieldNameForLookup + "." + Movies.id),
            Movies.title -> ("$" + fieldNameForLookup + "." + Movies.title),
            Movies.year -> ("$" + fieldNameForLookup + "." + Movies.year),
            Movies.posterUrl -> ("$" + fieldNameForLookup + "." + Movies.posterUrl),
            Movies.averageRating -> ("$" + fieldNameForLookup + "." + Movies.averageRating),
            Movies.viewers -> ("$" + fieldNameForLookup + "." + Movies.viewers),
            Movies.description -> ("$" + fieldNameForLookup + "." + Movies.description)
          )
        ),
        Match(
          BSONDocument(
            (Movies.id -> BSONDocument("$gt" -> lastId)) :: querySelector
          )
        ),
        Sort(
          sort.elements.map{
            case (field, value) if value.as[Int] == 1 => Ascending(field)
            case (field, value) if value.as[Int] == -1 => Descending(field)
          }
        ),
        Limit(limit)
      )
    }

    criteria match {
      case MovieCriteria(lastId, limit, titleContains, yearBetween, averageRating, viewers, haveSeen, sortBy) =>
        val and = ListBuffer[BSONElement]()
        titleContains.foreach(title =>
          and += "$text" -> BSONDocument("$search" -> title)
        )
        yearBetween.foreach(between =>
          and ++= betweenToQuery(Movies.year, between)
        )
        averageRating.foreach(between =>
          and ++= betweenToQuery(Movies.averageRating, between)
        )
        viewers.foreach(between =>
          and ++= betweenToQuery(Movies.viewers, between)
        )
        val querySelector = and.toList
        val sort = sortQuery(sortBy)

        haveSeen match {
          case Some(true) =>
            for {
              collection <- users
              pipeline = aggregationPipelineMyMovies(collection, lastId, limit, querySelector, sort)
              result <- collection.aggregate(pipeline.head, pipeline.tail).map(_.head[Movie])
            } yield result
          case _ =>
            for {
              collection <- movies
              result <- collection.find((Movies.id -> BSONDocument("$gt" -> lastId)) :: querySelector)
                          .sort(sort)
                          .cursor[Movie]()
                          .collect[Seq](limit, Cursor.FailOnError((seq: Seq[Movie], err) =>
                            Logger.error(
                              s"[RecommenderServiceDao] Fail during collectiong results from query($seq).",
                              err
                            )
                          ))
            } yield result
        }

    }
  }

  override def rateMovie(userId: Long, rating: Rating): Future[Unit] = {
    val writeResults =
      for {
        collection <- users
        result <- collection.update(
                    BSONDocument(Users.id -> userId),
                    BSONDocument("$addToSet" -> BSONDocument(Users.myRatings -> rating))
                  )
      } yield result
    flatMapWriteResult(writeResults)
  }

  override def updateRating(userId: Long, rating: Rating): Future[Unit] = {
    val writeResults =
      for {
        collection <- users
        result <- collection.update(
                    BSONDocument(Users.id -> userId, s"${Users.myRatings}.$$" -> rating.movieId),
                    BSONDocument("$set" -> BSONDocument(s"${Users.myRatings}.$$.${Ratings.rate}" -> rating.rate))
                  )
      } yield result
    flatMapWriteResult(writeResults)
  }

  override def getRecommendations(userId: Long): Future[Option[Seq[Rating]]] =
    for {
      collection <- users
      result <- collection.find(
                  BSONDocument(Users.id -> userId),
                  BSONDocument(Users.myRecommendations -> 1)
                ).one[List[Rating]]
    } yield result

  private def flatMapWriteResult(writeRes: Future[WriteResult]): Future[Unit] =
    writeRes.flatMap{
      case updateWriteResult if updateWriteResult.ok => Future.successful(Unit)
      case UpdateWriteResult(_, _, _, _, _ , _, _, msgOpt) =>
        msgOpt match {
          case Some(msg) => Future.failed(new RecommenderServiceDaoException(msg))
          case None => Future.failed(new RecommenderServiceDaoException("Error during updateRating!"))
        }
    }

  private def betweenToQuery[T](fieldName: String, between: BetweenInl[T])
                               (implicit writer: BSONWriter[T, BSONValue]): Seq[BSONElement] = between match {
    case BetweenInl(None, None) => Seq()
    case BetweenInl(Some(from), None) => Seq(fieldName -> BSONDocument("$gte" -> from))
    case BetweenInl(None, Some(to)) => Seq(fieldName -> BSONDocument("$lte" -> to))
    case BetweenInl(Some(from), Some(to)) =>
      Seq(
        fieldName-> BSONDocument("$gte" -> from),
        fieldName -> BSONDocument("$lte" -> to)
      )
  }
}

object RecommenderServiceDao {
  class RecommenderServiceDaoException(msg: String) extends Exception(msg)
}
