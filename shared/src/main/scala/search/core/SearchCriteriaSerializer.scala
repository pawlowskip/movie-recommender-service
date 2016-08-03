package search.core
import search.core.SearchCriteria.{And, Criteria, SearchProps}
import querystring.QueryString.{QueryStringParama, QueryStringRep}
import serialization.{Serializer, TokenConverter}
/**
  * Created by pp on 5/19/16.
  */
object SearchCriteriaSerializer {

  implicit def serializer[A](implicit tokenConverter: TokenConverter[QueryStringParama, A]): Serializer[SearchCriteria[A], QueryStringRep] =
    Serializer(criteria => criteria.toQueryString)

}
