package search.core
import search.core.SearchCriteria.{And, Criteria, SearchProps}
import querystring.QueryString.QueryStringRep
import serialization.Serializer
/**
  * Created by pp on 5/19/16.
  */
object SearchCriteriaSerializer {

  implicit def serializer[A]: Serializer[SearchCriteria[A], QueryStringRep] =
    Serializer(criteria => criteria.toQueryString)

}
