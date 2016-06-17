package search.core
import model.Movie
import search.core.SearchCriteria._
import serialization.DeserializationDefaults._
import serialization.{Deserializer, Fail, Ok}
import serialization.Deserializer.DeserializerBuilder._

/**
  * Created by pp on 5/27/16.
  */
object SearchCriteriaDeserializer {
  type Token = (String, String)
  type Header = (String, Int, Int)

  trait TokenConverter[T] {
    def convert(t: Token): T
  }

  implicit val tokenToString: TokenConverter[String] = new TokenConverter[String] {
    override def convert(t: (String, String)): String = t._2
  }

  def keyEqual(k: String): Token => Boolean = _._1 == k

  val limitDeserializer: Deserializer[Token, Int] = check[Token, Int](keyEqual("limit"))(_._2.toInt)
  val pageDeserializer: Deserializer[Token, Int] = check[Token, Int](keyEqual("page"))(_._2.toInt)

  val headerDeserializer: Deserializer[Token, Header] =
    single("criteria" -> "Movie", "Movie")
      .andThen[Int, (String, Int)](limitDeserializer)((s, i) => (s, i))
      .andThen[Int, Header](pageDeserializer)((a: (String, Int), b: Int) => (a._1, a._2, b))

  def equalDeserializer[T](implicit tokenConverter: TokenConverter[T]): Deserializer[Token, Equal[T]] =
    check[Token, Equal[T]](keyEqual("Equal"))(t => Equal[T](tokenConverter.convert(t)))


  val movieTitleEqualDes: Deserializer[Token, Field[String, Movie]] =
    single("field" -> "MovieTitle", Unit)
      .andThen(equalDeserializer[String])((_, eq) => Movie.MovieTitle(eq))

  val fieldDeserializer: Deserializer[Token, SearchCriteria[Movie]] = {
    oneOf(Seq(
      movieTitleEqualDes
    ))
  }

  val andDeserializer: Deserializer[Token, And[Movie]] =
    check[Token, Int](keyEqual("And"))(_._2.toInt)
      .flatMap {
        case i if i < 0 =>
          Deserializer.failed[Token, And[Movie]](s"And should contain positive number of criteria (passed $i).")
        case i =>
          processTimes[Token, SearchCriteria[Movie]](fieldDeserializer, i){
            case searchCriteria: Field[Movie, _] => true
            case _ => false
          }.map(And(_))
      }

  val bodyDeserializer: Deserializer[Token, SearchCriteria[Movie]] = andDeserializer

  val movieSearchCriteria: Deserializer[Token, Criteria[Movie]] =
    headerDeserializer
      .andThen[SearchCriteria[Movie], Criteria[Movie]](bodyDeserializer){(header, searchCriteria) =>
        val (name, limit, page) = header
        Criteria[Movie](searchCriteria).withName(name).limit(limit).page(page)
      }

}
