package querystring

import serialization.Deserializer.DeserializerBuilder
import serialization.{DeserializationDefaults, Deserializer, TokenConverter}
import upickle.default._
import upickle._

/**
  * Created by pp on 7/13/16.
  */
object QueryString {
  type QueryStringParama = (String, String)
  type QueryStringRep = Seq[QueryStringParama]

  def qsToString(qs: QueryStringParama): String = qs._2

  trait AsQueryString[A]{
    def toQueryString(implicit tokenConverter: TokenConverter[QueryStringParama, A]): QueryStringRep
  }

  def serializeValue[A](v: A)(implicit writer: Writer[A]): String = write[A](v)

  trait SimpleQueryStringSerialization[A] {
    type Token = QueryStringParama
    val value: A
    val paramaName: String = getClass.getSimpleName
    def serialize(implicit tokenConverter: TokenConverter[QueryStringParama, A]): Seq[QueryStringParama] =
      Seq(paramaName -> tokenConverter.convertFrom(value)._2)
  }

  implicit def readerToTokenConverter[A](implicit reader: Reader[A], writer: Writer[A]): TokenConverter[QueryStringParama, A] =
    new TokenConverter[QueryStringParama, A] {
      override def convertTo(value: QueryStringParama): A = read[A](QueryString.qsToString(value))(reader)
      override def convertFrom(value: A): (String, String) = ("", write[A](value))
    }

  def deserializerQS[T](defaultReader: Reader[T]): Deserializer[QueryStringParama, T] =
    DeserializerBuilder
      .singleWithNewToken[String, QueryStringParama, T](DeserializationDefaults.json[T](defaultReader), qsToString)

}
