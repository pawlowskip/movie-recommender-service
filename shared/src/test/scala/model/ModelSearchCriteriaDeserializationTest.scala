package model

import model.Movie.{MovieMyRating, MoviePoster, MovieViewers, _}
import model.Poster.PosterUrl
import search.core.SearchCriteria
import search.core.SearchCriteria.{apply => _, _}
import serialization.{Deserializer, Fail, Ok}
import utest.TestSuite
import utest._
import upickle.default._
import serialization.Deserializer.emptyInput
import querystring.QueryString.readerToTokenConverter

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

      val serializer = criteria.getSerializer
      val deserializer: Deserializer[(String, String), SearchCriteria[Movie]] = criteria.getDeserializer

      val serialized = serializer.serialize(criteria)
      val deserialized = deserializer.deserialize(serialized)


      // TODO check criteria in action on some collections, input criteria and this deserialized are almost different
      assert(deserialized == Ok(criteria, emptyInput, 40))
      deserialized match {
        case Ok(res, inputLeft, tokensParsed) =>
        case Fail(casue, inputLeft) =>
      }

    }
  }
}
