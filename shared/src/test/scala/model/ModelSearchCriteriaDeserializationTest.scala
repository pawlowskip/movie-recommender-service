package model

import model.Movie.{MovieMyRating, MoviePoster, MovieViewers, _}
import model.Poster.PosterUrl
import search.core.SearchCriteria.{apply => _, _}
import utest.TestSuite
import utest._

/**
  * Created by pp on 6/14/16.
  */
object ModelSearchCriteriaDeserializationTest extends TestSuite {
  //import SearchCriteria._

  val tests = this {
    "Test [1] - Movie" - {
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
      ).withName("Movie")




    }
  }
}
