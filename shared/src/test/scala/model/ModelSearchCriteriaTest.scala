package model
import model.Movie._
import model.Poster.PosterUrl
import search.core.SearchCriteria
import utest._

/**
  * Created by pp on 5/14/16.
  */
object ModelSearchCriteriaTest extends TestSuite {
  import SearchCriteria._

  val tests = this {

    "Test [1] - Poster" - {
      val criteria = Criteria[Poster](
        PosterUrl(MatchRegEx("^http://.+$"))
      )

      * - {assert(criteria.check(Poster("http://www.abc.com/poster")) == true)}
      * - {assert(criteria.check(Poster("www.abc.com/poster")) == false)}
    }

    "Test [2] - Movie" - {
      val criteria = Criteria[Movie](
        And(
          MovieTitle(Equal("Title")),
          MovieId(GreaterOrEqual(0)),
          MovieYear(Between(1999, 2016)),
          MovieAverageRating(GreaterThan(7.0)),
          MovieDescription(NotEmpty),
          MovieMyRating(Equal(None)),
          MovieViewers(GreaterThan(10000)),
          MoviePoster(PosterUrl(Equal("url")))
        )
      )

      val movie1 = Movie(
        id = 12,
        title = "Title",
        year = 2000,
        poster = Poster(url = "url"),
        averageRating = 8.4,
        myRating = None,
        viewers = 200000,
        description = "description"
      )

      * - {assert(criteria.check(movie1) == true)}
      * - {assert(criteria.check(movie1.copy(id = -1)) == false)}
    }

  }

}
