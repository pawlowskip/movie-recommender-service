package search.core

import model.Movie
import search.core.SearchCriteria._
import serialization.DeserializationDefaults._
import serialization.{Deserializer, Fail, Ok}
import serialization.Deserializer.DeserializerBuilder._
import serialization.Deserializer.TokenConverter
import querystring.QueryString

/**
  * Created by pp on 5/27/16.
  */
object SearchCriteriaDeserializer {
  type Token = QueryString.QueryStringParama
  type Header = (String, Int, Int)

  implicit val tokenToString: TokenConverter[Token, String] = (t: Token) => t._2


  def keyEqual(k: String): Token => Boolean = _._1 == k

  val limitDeserializer: Deserializer[Token, Int] = check[Token, Int](keyEqual("limit"))(_._2.toInt)
  val pageDeserializer: Deserializer[Token, Int] = check[Token, Int](keyEqual("page"))(_._2.toInt)

  def headerDeserializer(criteriaName: String): Deserializer[Token, Header] =
    single("criteria" -> criteriaName, criteriaName)
      .andThen[Int, (String, Int)](limitDeserializer)((s, i) => (s, i))
      .andThen[Int, Header](pageDeserializer)((a: (String, Int), b: Int) => (a._1, a._2, b))


  // combinators deserializer

  def multiValueDeserializer[T, Comb[T]](predicate: Token => Boolean,
                                         count: Token => Int,
                                         fieldDeserializer: Deserializer[Token, SearchCriteria[T]],
                                         success: Seq[SearchCriteria[T]] => Comb[T],
                                         fail: Int => Deserializer[Token, Comb[T]]): Deserializer[Token, Comb[T]] =
    check[Token, Int](predicate)(count)
      .flatMap {
        case i if i < 0 => fail(i)
        case i =>
          processTimes[Token, SearchCriteria[T]](fieldDeserializer, i) {
            case searchCriteria: Field[T, _] => true
            case _ => false
          }.map(success(_))
      }

  def singleValueDeserializer[T, Comb](predicate: Token => Boolean,
                                       deserializer: Deserializer[Token, T],
                                       transformer: T => Comb): Deserializer[Token, Comb] =
    for {
      _ <- check[Token, Token](predicate)(t => t)
      value <- deserializer
    } yield transformer(value)


  def andDeserializer[T](fieldDeserializer: Deserializer[Token, SearchCriteria[T]]): Deserializer[Token, And[T]] =
    multiValueDeserializer(
      keyEqual("And"),
      _._2.toInt,
      fieldDeserializer,
      seq => And(seq),
      i => Deserializer.failed[Token, And[T]](s"And should contain positive number of criteria (passed $i).")
    )

  def orDeserializer[T](fieldDeserializer: Deserializer[Token, SearchCriteria[T]]): Deserializer[Token, Or[T]] =
    multiValueDeserializer(
      keyEqual("Or"),
      _._2.toInt,
      fieldDeserializer,
      seq => Or(seq),
      i => Deserializer.failed[Token, Or[T]](s"Or should contain positive number of criteria (passed $i).")
    )

  def notDeserializer[T](innerDes: Deserializer[Token, SearchCriteria[T]]): Deserializer[Token, Not[T]] =
    check[Token, Token](keyEqual("Not"))(t => t)
      .flatMap {
        case t =>
          processTimes[Token, SearchCriteria[T]](innerDes, 1) {
            case _ => true
          }.map(seq => Not(seq.head))
      }

  def equalDeserializer[T](implicit tokenConverter: Deserializer[Token, T]): Deserializer[Token, Equal[T]] =
    singleValueDeserializer(keyEqual("Equal"), tokenConverter, Equal[T](_))

  def stringContainsDeserializer(implicit tokenConverter: Deserializer[Token, String]): Deserializer[Token, StringContains] =
    singleValueDeserializer(keyEqual("StringContains"), tokenConverter, StringContains(_))

  def seqContainsDeserializer[A, T <: Seq[A]](implicit tokenConverter: Deserializer[Token, A]): Deserializer[Token, SeqContains[A, T]] =
    singleValueDeserializer(keyEqual("SeqContains"), tokenConverter, SeqContains(_))

  def setContainsDeserializer[A, T <: Set[A]](implicit tokenConverter: Deserializer[Token, A]): Deserializer[Token, SetContains[A, T]] =
    singleValueDeserializer(keyEqual("SetContains"), tokenConverter, SetContains(_))

  def matchRegExDeserializer(implicit tokenConverter: Deserializer[Token, String]): Deserializer[Token, MatchRegEx] =
    singleValueDeserializer(keyEqual("MatchRegEx"), tokenConverter, MatchRegEx(_))

  def lessThanDeserializer[N](implicit tokenConverter: Deserializer[Token, N]): Deserializer[Token, LessThan[N]] =
    singleValueDeserializer(keyEqual("LessThan"), tokenConverter, LessThan[N](_))

  def lessOrEqualDeserializer[N](implicit tokenConverter: Deserializer[Token, N]): Deserializer[Token, LessOrEqual[N]] =
    singleValueDeserializer(keyEqual("LessOrEqual"), tokenConverter, LessOrEqual[N](_))

  def greaterThanDeserializer[N](implicit tokenConverter: Deserializer[Token, N]): Deserializer[Token, GreaterThan[N]] =
    singleValueDeserializer(keyEqual("GreaterThan"), tokenConverter, GreaterThan[N](_))

  def greaterOrEqualDeserializer[N](implicit tokenConverter: Deserializer[Token, N]): Deserializer[Token, GreaterOrEqual[N]] =
    singleValueDeserializer(keyEqual("GreaterOrEqual"), tokenConverter, GreaterOrEqual[N](_))

  def inDeserializer[N](implicit tokenConverter: Deserializer[Token, Seq[N]]): Deserializer[Token, In[N]] =
    singleValueDeserializer(keyEqual("In"), tokenConverter, In(_))




  val movieTitleEqualDes: Deserializer[Token, Field[String, Movie]] =
    single("field" -> "MovieTitle", Unit)
      .andThen(equalDeserializer[String])((_, eq) => Movie.MovieTitle(eq))

  val fieldDeserializer: Deserializer[Token, SearchCriteria[Movie]] = {
    oneOf(Seq(
      movieTitleEqualDes
    ))
  }


  val bodyDeserializer: Deserializer[Token, SearchCriteria[Movie]] = andDeserializer(fieldDeserializer)

  val movieSearchCriteria: Deserializer[Token, Criteria[Movie]] =
    headerDeserializer("Movie")
      .andThen[SearchCriteria[Movie], Criteria[Movie]](bodyDeserializer) { (header, searchCriteria) =>
      val (name, limit, page) = header
      Criteria[Movie](searchCriteria).withName(name).limit(limit).page(page)
    }

}
