package search.core
import querystring.Serializer
import search.core.SearchCriteria.{And, Criteria, SearchProps}
import search.core.SearchCriteria.QueryString
/**
  * Created by pp on 5/19/16.
  */
object SearchCriteriaSerializer {

  implicit def serializer[A](searchCriteria: SearchCriteria[A]): Serializer[SearchCriteria[A], QueryString] =
    Serializer(criteria => criteria.toQueryString)

}
