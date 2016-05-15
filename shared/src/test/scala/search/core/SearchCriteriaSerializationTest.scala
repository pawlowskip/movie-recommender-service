package search.core
//import model.{Movie, Poster}
//import model.Movie.{MovieMyRating, MoviePoster, MovieViewers, _}
//import model.Poster.PosterUrl
import utest._
import upickle.default._
import search.core.Collections._

/**
  * Created by pp on 5/14/16.
  */
object SearchCriteriaSerializationTest extends TestSuite {
  //import SearchCriteria._

  val tests = this {
    "Test [1] - Serialization" - {

      /*val movie1 = Movie(
        id = 12,
        title = "Title",
        year = 2000,
        poster = Poster(url = "url"),
        averageRating = 8.4,
        myRating = None,
        viewers = 200000,
        description = "description"
      )*/



        /*
        And(
          MovieTitle(Equal("Title")),
          MovieId(GreaterOrEqual(0)),
          MovieYear(Between(1999, 2016)),
          MovieAverageRating(GreaterThan(7.0)),
          MovieDescription(NotEmpty),
          MovieMyRating(Equal(None)),
          MovieViewers(GreaterThan(10000)),
          MoviePoster(PosterUrl(Equal("url")))
        )*/



      //val pickled = write(criteria)
      //val unpickled = read[Big](pickled)

      val s = SetOfSomething(Set(Something(1, "1"), Something(2, "2")), "set")
      read[S](write(s)) ==> s
      readJs[S](doSomething(Something(1, "1"), s)) ==> s

      val c =
        TupleCriteria(GreaterThan(1))

      read[Criteria](write(c)) ==> c

      //* - {assert(criteria == unpickled)}
      //* - {assert(criteria.check(1) == unpickled.check(1))}
    }

  }
}
