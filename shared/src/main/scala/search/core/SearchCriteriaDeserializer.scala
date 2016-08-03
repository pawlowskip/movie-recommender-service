package search.core

import model.Movie
import search.core.SearchCriteria._
import serialization.DeserializationDefaults._
import serialization.{Deserializer, Fail, Ok, TokenConverter}
import serialization.Deserializer.DeserializerBuilder._
import querystring.QueryString
import querystring.QueryString._

/**
  * Created by pp on 5/27/16.
  */
object SearchCriteriaDeserializer {
  type Token = QueryString.QueryStringParama
  type Header = (String, Int, Int)

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
          processTimes[Token, SearchCriteria[T]](fieldDeserializer, i) { // Can works without this always true
            case searchCriteria: Field[T, _] => true
            case _ => false
          }.map(success(_))
      }

  def singleValueDeserializer[T, Comb](predicate: Token => Boolean,
                                       converter: TokenConverter[Token, T],
                                       transformer: T => Comb): Deserializer[Token, Comb] =
    for {
      value <- check[Token, T](predicate)(t => converter.convertTo(t))
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


  def equalDeserializer[T](tokenConverter: TokenConverter[Token, T]): Deserializer[Token, Equal[T]] =
    singleValueDeserializer[T, Equal[T]](keyEqual("Equal"), tokenConverter, Equal[T](_))

  def stringContainsDeserializer(tokenConverter: TokenConverter[Token, String]): Deserializer[Token, StringContains] =
    singleValueDeserializer[String, StringContains](keyEqual("StringContains"), tokenConverter, StringContains(_))

  def seqContainsDeserializer[A, T <: Seq[A]](tokenConverter: TokenConverter[Token, A]): Deserializer[Token, SeqContains[A, T]] =
    singleValueDeserializer[A, SeqContains[A, T]](keyEqual("SeqContains"), tokenConverter, SeqContains(_)(tokenConverter))

  def setContainsDeserializer[A, T <: Set[A]](tokenConverter: TokenConverter[Token, A]): Deserializer[Token, SetContains[A, T]] =
    singleValueDeserializer[A, SetContains[A, T]](keyEqual("SetContains"), tokenConverter, SetContains(_)(tokenConverter))

  def matchRegExDeserializer(tokenConverter: TokenConverter[Token, String]): Deserializer[Token, MatchRegEx] =
    singleValueDeserializer[String, MatchRegEx](keyEqual("MatchRegEx"), tokenConverter, MatchRegEx(_))

  def lessThanDeserializer[N](tokenConverter: TokenConverter[Token, N])(implicit ord: Ordering[N]): Deserializer[Token, LessThan[N]] =
    singleValueDeserializer[N, LessThan[N]](keyEqual("LessThan"), tokenConverter, LessThan[N](_))

  def lessOrEqualDeserializer[N](tokenConverter: TokenConverter[Token, N])(implicit ord: Ordering[N]): Deserializer[Token, LessOrEqual[N]] =
    singleValueDeserializer[N, LessOrEqual[N]](keyEqual("LessOrEqual"), tokenConverter, LessOrEqual[N](_))

  def greaterThanDeserializer[N](tokenConverter: TokenConverter[Token, N])(implicit ord: Ordering[N]): Deserializer[Token, GreaterThan[N]] =
    singleValueDeserializer[N, GreaterThan[N]](keyEqual("GreaterThan"), tokenConverter, GreaterThan[N](_))

  def greaterOrEqualDeserializer[N](tokenConverter: TokenConverter[Token, N])(implicit ord: Ordering[N]): Deserializer[Token, GreaterOrEqual[N]] =
    singleValueDeserializer[N, GreaterOrEqual[N]](keyEqual("GreaterOrEqual"), tokenConverter, GreaterOrEqual[N](_))

  def fieldDeserializer[F, C](name: String,
                              des: Deserializer[QueryStringParama, SearchCriteria[F]],
                              f: C => F,
                              tokenConverter: TokenConverter[QueryStringParama, F])

  : Deserializer[QueryStringParama, SearchCriteria[C]] = {

    single("field" -> name, Unit)
      .andThen[SearchCriteria[F], SearchCriteria[C]](des)((_, sf) => new Field(f)(tokenConverter) {
      override val criteria: SearchCriteria[F] = sf
    })
  }

  def searchCriteria[A](name: String, bodyDeserializer: Deserializer[Token, SearchCriteria[A]]): Deserializer[Token, Criteria[A]] =
    headerDeserializer(name)
      .andThen[SearchCriteria[A], Criteria[A]](bodyDeserializer) { (header, searchCriteria) =>
      val (name, limit, page) = header
      Criteria[A](searchCriteria).withName(name).limit(limit).page(page)
    }

}
