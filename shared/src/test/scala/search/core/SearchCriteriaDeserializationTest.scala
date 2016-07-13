package search.core

import model.Movie
import model.Movie.{MovieMyRating, MoviePoster, MovieViewers, _}
import model.Poster.PosterUrl
import search.core.SearchCriteria._
import serialization.Deserializer.{apply => _, _}
import serialization.Ok
import utest.TestSuite
import utest._
/**
  * Created by pp on 7/12/16.
  */
object SearchCriteriaDeserializationTest extends TestSuite {
  //import SearchCriteria._

  val tests = this {
    "Test [1] - deserialization" - {
      val criteria = Criteria[Movie](
        And(
          MovieTitle(Equal("Title"))
        )
      ).withName("Movie")



      val serialized = SearchCriteriaSerializer.serializer.serialize(criteria)
      val deserialized = SearchCriteriaDeserializer.movieSearchCriteria.deserialize(serialized)

      assert(deserialized == Ok(criteria, emptyInput[QueryStringParama], 6))
    }
  }
}