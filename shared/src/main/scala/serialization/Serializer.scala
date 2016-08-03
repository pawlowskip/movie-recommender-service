package serialization

import upickle.default._
/**
  * Created by pp on 5/18/16.
  */
case class Serializer[A, B](f: A => B) {
  def serialize(a: A): B = f(a)
}

object Serializer {

  def string[A](a: A): Serializer[A, String] = Serializer(a => a.toString)

  def json[A](implicit writer: upickle.default.Writer[A]): Serializer[A, String] = Serializer(write[A](_))

}

trait SerializableAs[Rep, Token, A, Res] {
  def getSerializer(implicit tokenConverter: TokenConverter[Token, A]): Serializer[Res, Rep]
}

