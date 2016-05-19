package querystring
import upickle.default._
/**
  * Created by pp on 5/18/16.
  */
case class Serializer[A, B](f: A => B) {
  def serialize(a: A): B = f(a)
}

object Serializer {

  def string[A](a: A): Serializer[A, String] = Serializer(a => a.toString)

  def json[A](a: A): Serializer[A, String] = Serializer(a => write(a))

}

case class Deserializer[I <: Iterable[Token], Token, O](f: I => O) {
  def deserialize(a: I): O = f(a)
  def mapI[A <: Iterable[NewToken], NewToken](g: A => I): Deserializer[A, NewToken, O] = Deserializer(f.compose(g))
  def mapO[A](g: O => A): Deserializer[I, Token, A] = Deserializer(f.andThen(g))
}

object Deserializer {

  val str: Deserializer[Int, Char, Int] = new Deserializer[Int, Char, Int](i => i)

  def success[A](a: A)

  def process[T <: Iterable[Token], Token, O](data: T)(deserializer: Deserializer[T, Token, O]) = {

  }

  def string(s: String): =

}

