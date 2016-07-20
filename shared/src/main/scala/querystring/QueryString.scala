package querystring

import search.core.SearchCriteria
import serialization.Deserializer.DeserializerBuilder
import serialization.{DeserializationDefaults, Deserializer}

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

  def deserializerQS[T]: Deserializer[QueryStringParama, T] =
    DeserializerBuilder.transform[QueryStringParama, String](_._2)

    Deserializer[QueryStringParama, T]{ input =>
    input.headOption match {
      case Some(token) => DeserializationDefaults.json[A]
      case None =>
    }
  }

}
