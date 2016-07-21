package querystring

import search.core.SearchCriteria
import serialization.Deserializer.DeserializerBuilder
import serialization.{DeserializationDefaults, Deserializer}
import upickle.default._

/**
  * Created by pp on 7/13/16.
  */
object QueryString {
  type QueryStringParama = (String, String)
  type QueryStringRep = Seq[QueryStringParama]

  def toString(qs: QueryStringParama): String = qs._2

  trait AsQueryString {
    def toQueryString: QueryStringRep
  }

  trait SimpleQueryStringSerialization[A] extends AsQueryString {
    val value: A
    val paramaName: String = getClass.getSimpleName
    def toQueryString: Seq[QueryStringParama] = Seq(paramaName -> value.toString)
  }

  def deserializerQS[T](defaultReader: Reader[T]): Deserializer[QueryStringParama, T] =
    DeserializerBuilder.singleWithNewToken[String, QueryStringParama, T](DeserializationDefaults.json[T], toString)

}
