package search.core

import querystring.QueryString
import search.core.SearchCriteriaDeserializer._
import querystring.QueryString._
import serialization.Deserializer.DeserializerBuilder
import serialization.Deserializer.DeserializerBuilder._
import serialization._

import scala.annotation.tailrec
import upickle.default._

/**
  * Created by pp on 5/7/16.
  */
sealed trait SearchCriteria[Val]
  extends AsQueryString[Val]
    with SerializableAs[QueryStringRep, QSParam, Val, SearchCriteria[Val]]
    with DeserializableAs[QSParam, Val, SearchCriteria[Val]] {

  def check(value: Val): Boolean
  
  override def getSerializer(implicit tokenConverter: TokenConverter[QSParam, Val]) =
    SearchCriteriaSerializer.serializer(tokenConverter)
}

object SearchCriteria {

  def apply[A](f: A => Boolean,
               toQueryStringFun: => Seq[QSParam] = Seq(),
               deserializer: Deserializer[QSParam, SearchCriteria[A]] =
                  Deserializer.failed[QSParam, SearchCriteria[A]]("Not implemented!")
              ) =
    new SearchCriteria[A] {
      override def check(value: A) = f(value)
      override def getDeserializer(implicit converter: TokenConverter[QSParam, A]):
        Deserializer[QSParam, SearchCriteria[A]] = deserializer
      override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): QueryStringRep =
        toQueryStringFun
    }

  case class SearchProps(limit: Int, page: Int)

  case class Criteria[A](criteria: SearchCriteria[A],
                         queryStringName: String = "",
                         props: Option[SearchProps] = None) extends SearchCriteria[A] {

    private val defaultLimit = 10

    override def check(value: A): Boolean = criteria.check(value)

    private def checkProps: Criteria[A] = props match {
      case Some(SearchProps(-1, -1)) => this.copy(props = None)
      case _ => this
    }

    def limit(i: Int): Criteria[A] = this.copy(props = Some(SearchProps(i, 0))).checkProps

    def page(p: Int): Criteria[A] = props match {
      case None => this.copy(props = Some(SearchProps(defaultLimit, p)))
      case Some(SearchProps(l, _)) => this.copy(props = Some(SearchProps(l, p))).checkProps
      case _ => this.copy(props = Some(SearchProps(defaultLimit, p)))
    }

    def withName(name: String): Criteria[A] = copy(queryStringName = name)

    def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      params += "criteria" -> s"""$queryStringName"""
      props match {
        case Some(SearchProps(limit, page)) =>
          params += "limit" -> limit.toString
          params += "page" -> page.toString
        case None =>
          params += "limit" -> "-1"
          params += "page" -> "-1"
      }
      params ++= criteria.toQueryString
      params.toList
    }

    override def getDeserializer(implicit converter: TokenConverter[QSParam, A]): Deserializer[QSParam, SearchCriteria[A]] =
      searchCriteriaDeserializer(queryStringName, criteria.getDeserializer(converter))
  }

  abstract class Field[F, C](f: C => F)(implicit additionalTokenConverter: TokenConverter[QSParam, F]) extends SearchCriteria[C] {

    val criteria: SearchCriteria[F]

    override def check(value: C): Boolean = criteria.check(f(value))

    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, C]): Seq[QSParam] =
      Seq("field" -> s""""${getClass.getSimpleName}"""") ++ criteria.toQueryString

    override def getDeserializer(implicit tokenConverter: TokenConverter[QSParam, C]): Deserializer[QSParam, SearchCriteria[C]] =
      SearchCriteriaDeserializer.fieldDeserializer(getClass.getSimpleName, criteria.getDeserializer(additionalTokenConverter), f, additionalTokenConverter)
  }


  implicit def toFilterExecutor[A](criteria: SearchCriteria[A]): FilterExecutor[A] = new FilterExecutor(criteria)

  class FilterExecutor[A](criteria: SearchCriteria[A]) {
    def filter[C <: Iterable[A]](collection: C): Iterable[A] = criteria match {
      case Criteria(searchCriteria, _, None) => collection.filter(a => searchCriteria.check(a))
      case Criteria(searchCriteria, _, Some(SearchProps(limit, page))) =>
        collection.filter(a => searchCriteria.check(a)).slice(page * limit, (page + 1) * limit)
      case _ => collection.filter(p => criteria.check(p))
    }
  }


  case class And[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.forall(_.check(value))
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      params += "And" -> criteria.size.toString
      criteria.foreach(c => params ++= c.toQueryString)
      params.toList
    }

    override def getDeserializer(implicit converter: TokenConverter[QSParam, A]): Deserializer[QSParam, And[A]] =
      SearchCriteriaDeserializer.andDeserializer{
        oneOf(criteria.map(_.getDeserializer(converter)))
      }
  }

  object And {
    def apply[A](criteria: SearchCriteria[A]*): SearchCriteria[A] = And(criteria)
  }

  case class Or[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.exists(_.check(value))
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      params += "Or" -> criteria.size.toString
      criteria.foreach(c => params ++= c.toQueryString)
      params.toList
    }

    override def getDeserializer(implicit converter: TokenConverter[QSParam, A]): Deserializer[QSParam, Or[A]] =
      SearchCriteriaDeserializer.orDeserializer{
        oneOf(criteria.map(_.getDeserializer(converter)))
      }
  }

  object Or {
    def apply[A](criteria: SearchCriteria[A]*): SearchCriteria[A] = Or(criteria)
  }

  case class Not[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = !criteria.check(value)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): Seq[QSParam] = {
      val params = collection.mutable.ListBuffer[QSParam]()
      Seq("Not" -> "1")
      params ++= criteria.toQueryString
      params.toList
    }

    def getDeserializer(implicit converter: TokenConverter[QSParam, A]): Deserializer[QSParam, Not[A]] =
      SearchCriteriaDeserializer.notDeserializer(criteria.getDeserializer(converter))
  }

  case class Equal[A](value: A) extends SearchCriteria[A] with SimpleQueryStringSerialization[A] {
    override def check(value: A): Boolean = this.value == value
    override def getDeserializer(implicit converter: TokenConverter[QSParam, A]): Deserializer[QSParam, Equal[A]] =
      SearchCriteriaDeserializer.equalDeserializer[A](converter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, A]): QueryStringRep = serialize
  }

  trait ContainsInvoker[C, A] {
    def contains(a: A): SearchCriteria[C]
  }

  def Contains[C, A](value: A)(implicit containsInvoker: ContainsInvoker[C, A],
                               additionalTokenConverter: TokenConverter[QSParam, A]): SearchCriteria[C] = {
    containsInvoker.contains(value)
  }

  implicit val stringContainsInvoker: ContainsInvoker[String, String] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  implicit def seqContainsInvoker[Elem, Col <: Seq[Elem]]
    (implicit additionalTokenConverter: TokenConverter[QSParam, Elem]): ContainsInvoker[Col, Elem] = new ContainsInvoker[Col, Elem] {

    override def contains(value: Elem): SearchCriteria[Col] = SeqContains[Elem, Col](value)
  }

  implicit def setContainsInvoker[Elem, Col <: Set[Elem]]
  (implicit additionalTokenConverter: TokenConverter[QSParam, Elem]): ContainsInvoker[Col, Elem] = new ContainsInvoker[Col, Elem] {

    override def contains(value: Elem): SearchCriteria[Col] = SetContains[Elem, Col](value)
  }

  case class StringContains(value: String) extends SearchCriteria[String] with SimpleQueryStringSerialization[String] {
    override def check(value: String): Boolean = value.contains(this.value)
    override def getDeserializer(implicit converter: TokenConverter[QSParam, String]): Deserializer[QSParam, StringContains] =
      SearchCriteriaDeserializer.stringContainsDeserializer(converter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, String]): QueryStringRep = serialize
  }

  case class SeqContains[A, T <: Seq[A]](value: A)(implicit additionalTokenConverter: TokenConverter[QSParam, A])
    extends SearchCriteria[T] with SimpleQueryStringSerialization[A] {

    override def check(value: T): Boolean = value.contains(this.value)
    override def getDeserializer(implicit converter: TokenConverter[QSParam, T]): Deserializer[QSParam, SeqContains[A, T]] =
      SearchCriteriaDeserializer.seqContainsDeserializer[A, T](additionalTokenConverter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, T]): QueryStringRep =
      serialize(additionalTokenConverter)
  }

  case class SetContains[A, T <: Set[A]](value: A)(implicit additionalTokenConverter: TokenConverter[QSParam, A])
    extends SearchCriteria[T] with SimpleQueryStringSerialization[A] {

    override def check(value: T): Boolean = value.contains(this.value)
    override def getDeserializer(implicit converter: TokenConverter[QSParam, T]): Deserializer[QSParam, SetContains[A, T]] =
      SearchCriteriaDeserializer.setContainsDeserializer[A, T](additionalTokenConverter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, T]): QueryStringRep =
      serialize(additionalTokenConverter)
  }


  case class MatchRegEx(value: String) extends SearchCriteria[String] with SimpleQueryStringSerialization[String] {
    val regEx = value.r
    override def check(value: String): Boolean = regEx.findFirstIn(value).isDefined
    override def getDeserializer(implicit converter: TokenConverter[QSParam, String]): Deserializer[QSParam, MatchRegEx] =
      SearchCriteriaDeserializer.matchRegExDeserializer(converter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, String]): QueryStringRep = serialize
  }

  case class LessThan[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.lt(value, this.value)

    override def getDeserializer(implicit converter: TokenConverter[QSParam, N]): Deserializer[QSParam, LessThan[N]] =
      SearchCriteriaDeserializer.lessThanDeserializer[N](converter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, N]): QueryStringRep = serialize
  }

  case class LessOrEqual[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
       with SimpleQueryStringSerialization[N] {
    override def check(value: N): Boolean = ordering.lteq(value, this.value)
    override def getDeserializer(implicit converter: TokenConverter[QSParam, N]): Deserializer[QSParam, LessOrEqual[N]] =
      SearchCriteriaDeserializer.lessOrEqualDeserializer[N](converter)
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, N]): QueryStringRep = serialize
  }

  case class GreaterThan[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.gt(value, this.value)

    override def getDeserializer(implicit converter: TokenConverter[QSParam, N]): Deserializer[QSParam, GreaterThan[N]] =
      SearchCriteriaDeserializer.greaterThanDeserializer[N](converter)

    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, N]): QueryStringRep = serialize
  }

  case class GreaterOrEqual[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.gteq(value, this.value)

    override def getDeserializer(implicit converter: TokenConverter[QSParam, N]): Deserializer[QSParam, GreaterOrEqual[N]] =
      SearchCriteriaDeserializer.greaterOrEqualDeserializer[N](converter)

    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, N]): QueryStringRep = serialize
  }

  object In {
    def apply[A](criteria: Seq[A]): SearchCriteria[A] = Or(criteria.map(Equal(_)): _*)
  }

  object Between{
    def apply[A](from: A, to: A)(implicit ordering: Ordering[A]) : SearchCriteria[A] =
      And(
        GreaterThan(from)(ordering),
        LessThan(to)(ordering)
      )
  }

  trait NotEmptyInvoker[T]{
    def notEmpty: SearchCriteria[T]
  }

  def NotEmpty[T](implicit notEmptyInvoker: NotEmptyInvoker[T]): SearchCriteria[T] = notEmptyInvoker.notEmpty
  def IsEmpty[T](implicit notEmptyInvoker: NotEmptyInvoker[T]): SearchCriteria[T] = Not(notEmptyInvoker.notEmpty)


  implicit val nonEmptyString: NotEmptyInvoker[String] = new NotEmptyInvoker[String] {
    override def notEmpty: SearchCriteria[String] = NotEmptyString
  }

  implicit def nonEmptyCollection[T <: Traversable[_]]: NotEmptyInvoker[T] = new NotEmptyInvoker[T] {
    override def notEmpty: SearchCriteria[T] = NotEmptyCollection()
  }

  case object NotEmptyString extends SearchCriteria[String] {
    override def check(value: String): Boolean = value.nonEmpty
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, String]): Seq[QSParam] =
      Seq(representation)
    override def getDeserializer(implicit converter: TokenConverter[QSParam, String])
      : Deserializer[QSParam, SearchCriteria[String]] =
      DeserializerBuilder.single(representation, this)

    private val representation = "NotEmptyString" -> "1"
  }

  case class NotEmptyCollection[T <: Traversable[_]]() extends SearchCriteria[T] {
    override def check(value: T): Boolean = value.nonEmpty
    override def toQueryString(implicit tokenConverter: TokenConverter[QSParam, T]): Seq[QSParam] =
      Seq(representation)
    override def getDeserializer(implicit converter: TokenConverter[QSParam, T])
      : Deserializer[QSParam, SearchCriteria[T]] =
      DeserializerBuilder.single(representation, this)

      private val representation = "NotEmptyCollection" -> "1"
  }


}

