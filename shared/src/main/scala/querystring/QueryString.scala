package querystring

import search.core.SearchCriteria
import serialization.Deserializer

/**
  * Created by pp on 7/13/16.
  */
object QueryString {
  type QueryStringParama = (String, String)
  type QueryStringRep = Seq[QueryStringParama]

  trait AsQueryString {
    def toQueryString: QueryStringRep
  }

  trait SimpleQueryStringSerialization[A] extends AsQueryString {
    val value: A
    val paramaName: String = getClass.getSimpleName
    def toQueryString: Seq[QueryStringParama] = Seq(paramaName -> value.toString)
  }

  def deserializerFromSimpleQSS[A, T <: SimpleQueryStringSerialization[A]]: Deserializer[QueryStringParama, T] = {

  }

}
