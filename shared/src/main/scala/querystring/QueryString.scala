package querystring

import serialization.Deserializer.DeserializerBuilder
import serialization.{DeserializationDefaults, Deserializer, TokenConverter}
import upickle.default._
import upickle._

/**
  * Created by pp on 7/13/16.
  */
object QueryString {
  type QSParam = (String, String)
  type QueryStringRep = Seq[QSParam]

  def qsToString(qs: QSParam): String = qs._2

  trait AsQueryString[A]{
    def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): QueryStringRep
  }

  def serializeValue[A](v: A)(implicit writer: Writer[A]): String = write[A](v)

  trait SimpleQueryStringSerialization[A] {
    type Token = QSParam
    val value: A
    val paramaName: String = getClass.getSimpleName
    def serialize(implicit tokenConverter: TokenConverter[QSParam, A]): Seq[QSParam] =
      Seq(paramaName -> tokenConverter.convertFrom(value)._2)
  }

  implicit def readerToTokenConverter[A](implicit reader: Reader[A], writer: Writer[A]): TokenConverter[QSParam, A] =
    new TokenConverter[QSParam, A] {
      override def convertTo(value: QSParam): A = read[A](QueryString.qsToString(value))(reader)
      override def convertFrom(value: A): (String, String) = ("", write[A](value))
    }

  def deserializerQS[T](defaultReader: Reader[T]): Deserializer[QSParam, T] =
    DeserializerBuilder
      .singleWithNewToken[String, QSParam, T](DeserializationDefaults.json[T](defaultReader), qsToString)

}
