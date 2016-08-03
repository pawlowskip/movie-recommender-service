package search.core

import model.Movie
import model.Movie.{MovieMyRating, MoviePoster, MovieViewers, _}
import model.Poster.PosterUrl
import utest._
import search.core.SearchCriteria._
import utest._
import upickle.default._
import querystring.QueryString.readerToTokenConverter
/**
  * Created by pp on 5/14/16.
  */
object SearchCriteriaSerializationTest extends TestSuite {
  //import SearchCriteria._

  val tests = this {
    "Test [1] - Serialization" - {
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

      val queryString = criteria.toQueryString
      val correctAnswer: Seq[(String, String)] =
        List(
          ("criteria", "\"Movie\""),
          ("limit", "-1"),
          ("page", "-1"),
          ("And", "8"),
          ("field", "\"MovieTitle\""),
          ("Equal", "\"Title\""),
          ("field", "\"MovieId\""),
          ("GreaterOrEqual", "0"),
          ("field", "\"MovieYear\""),
          ("And","2"), ("GreaterThan","1999"), ("LessThan","2016"),
          ("field", "\"MovieAverageRating\""),
          ("GreaterThan", "7"),
          ("field", "\"MovieDescription\""),
          ("NotEmptyString", "1"),
          ("field", "\"MovieMyRating\""),
          ("Equal", "[]"),
          ("field", "\"MovieViewers\""),
          ("GreaterThan", "10000"),
          ("field" ,"\"MoviePoster\""),
          ("field","\"PosterUrl\""),
          ("Equal", "\"url\"")
        )

      assert(queryString == correctAnswer)
    }

  }
}
