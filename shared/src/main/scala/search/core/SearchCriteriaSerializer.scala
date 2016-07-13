package search.core
import search.core.SearchCriteria.{And, Criteria, SearchProps}
import search.core.SearchCriteria.QueryString
import serialization.Serializer
/**
  * Created by pp on 5/19/16.
  */
object SearchCriteriaSerializer {

  implicit def serializer[A]: Serializer[SearchCriteria[A], QueryString] =
    Serializer(criteria => criteria.toQueryString)

}
