package search.core
import model.Movie
import search.core.SearchCriteria.Criteria
import serialization.DeserializationDefaults._
import serialization.Deserializer
import serialization.Deserializer.DeserializerBuilder._
/**
  * Created by pp on 5/27/16.
  */
object SearchCriteriaDeserializer {
  type Token = (String, String)
  val movieSearchCriteria: Deserializer[Token, Criteria[Movie]] =
    foldBackward(
      foldBackward(
        single[Token, SearchCriteria[Movie] => Criteria[Movie]]
          (("criteria", "Movie"), searchCriteria => Criteria[Movie](searchCriteria, "Movie")),

    )

}
