package search.core

import querystring.QueryString._
import serialization.Deserializer.DeserializerBuilder._
import serialization.{DeserializableAs, DeserializationDefaults, Deserializer, SerializableAs}

import scala.annotation.tailrec

/**
  * Created by pp on 5/7/16.
  */
sealed trait SearchCriteria[A]
  extends AsQueryString
    with SerializableAs[SearchCriteria[A], QueryStringRep]
    with DeserializableAs[QueryStringParama, SearchCriteria[A]] {

  def check(value: A): Boolean
  def toQueryString: Seq[QueryStringParama]
  def getSerializer = SearchCriteriaSerializer.serializer
}

object SearchCriteria {

  def apply[A](f: A => Boolean,
               toQueryStringFun: => Seq[QueryStringParama] = Seq(),
               deserializer: Deserializer[(String, String), SearchCriteria[A]] = Deserializer.failed("Not implemented!")) = new SearchCriteria[A] {
    override def check(value: A) = f(value)
    override def toQueryString: Seq[QueryStringParama] = toQueryStringFun
    override def getDeserializer: Deserializer[(String, String), SearchCriteria[A]] = deserializer

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

    def toQueryString: Seq[(String, String)] = {
      val params = collection.mutable.ListBuffer[(String, String)]()
      params += "criteria" -> queryStringName
      props match {
        case Some(SearchProps(limit, page)) => params += "limit" -> limit.toString; params += "page" -> page.toString
        case None => params += "limit" -> "-1"; params += "page" -> "-1"
      }
      params ++= criteria.toQueryString
      params.toList
    }

    override def getDeserializer: Deserializer[(String, String), SearchCriteria[A]] = SearchCriteriaDeserializer.
  }

  abstract class Field[F, C](f: C => F) extends SearchCriteria[C] {
    val criteria: SearchCriteria[F]
    override def check(value: C): Boolean = criteria.check(f(value))
    def toQueryString: Seq[QueryStringParama] = Seq("field" -> getClass.getSimpleName) ++ criteria.toQueryString
  }

  implicit def toFilterExecutor[A](criteria: Criteria[A]): FilterExecutor[A] = new FilterExecutor(criteria)

  class FilterExecutor[A](criteria: Criteria[A]) {
    def filter[C <: Iterable[A]](collection: C): Iterable[A] = criteria match {
      case Criteria(searchCriteria, _, None) => collection.filter(a => searchCriteria.check(a))
      case Criteria(searchCriteria, _, Some(SearchProps(limit, page))) =>
        collection.filter(a => searchCriteria.check(a)).slice(page * limit, (page + 1) * limit)
    }
  }


  case class And[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.forall(_.check(value))
    override def toQueryString: Seq[QueryStringParama] = {
      val params = collection.mutable.ListBuffer[QueryStringParama]()
      params += "And" -> criteria.size.toString
      criteria.foreach(c => params ++= c.toQueryString)
      params.toList
    }

    override def getDeserializer: Deserializer[(String, String), And[A]] =
      SearchCriteriaDeserializer.andDeserializer{
        oneOf(criteria.map(_.getDeserializer))
      }
  }

  object And {
    def apply[A](criteria: SearchCriteria[A]*): SearchCriteria[A] = And(criteria)
  }

  case class Or[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.exists(_.check(value))
    override def toQueryString: Seq[QueryStringParama] = {
      val params = collection.mutable.ListBuffer[QueryStringParama]()
      params += "Or" -> criteria.size.toString
      criteria.foreach(c => params ++= c.toQueryString)
      params.toList
    }

    override def getDeserializer: Deserializer[(String, String), Or[A]] =
      SearchCriteriaDeserializer.orDeserializer{
        oneOf(criteria.map(_.getDeserializer))
      }
  }

  object Or {
    def apply[A](criteria: SearchCriteria[A]*): SearchCriteria[A] = Or(criteria)
  }

  case class Not[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = !criteria.check(value)
    override def toQueryString: Seq[(String, String)] = {
      val params = collection.mutable.ListBuffer[QueryStringParama]()
      Seq("Not" -> "1")
      params ++= criteria.toQueryString
      params.toList
    }

    override def getDeserializer: Deserializer[(String, String), Not[A]] =
      SearchCriteriaDeserializer.notDeserializer(criteria.getDeserializer)
  }


  case class Equal[A](value: A) extends SearchCriteria[A] with SimpleQueryStringSerialization[A] {
    override def check(value: A): Boolean = this.value == value
    override def getDeserializer: Deserializer[(String, String), Equal[A]] =
      SearchCriteriaDeserializer.equalDeserializer[A](DeserializationDefaults.json[A])
  }

  trait ContainsInvoker[C, A] {
    def contains(a: A): SearchCriteria[C]
  }

  def Contains[C, A](value: A)(implicit containsInvoker: ContainsInvoker[C, A]): SearchCriteria[C] = {
    containsInvoker.contains(value)
  }

  implicit val stringContainsInvoker: ContainsInvoker[String, String] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  implicit def seqContainsInvoker[Elem, Col <: Seq[Elem]]: ContainsInvoker[Col, Elem] = new ContainsInvoker[Col, Elem] {
    override def contains(value: Elem): SearchCriteria[Col] = SeqContains[Elem, Col](value)
  }

  implicit def setContainsInvoker[Elem, Col <: Set[Elem]]: ContainsInvoker[Col, Elem] = new ContainsInvoker[Col, Elem] {
    override def contains(value: Elem): SearchCriteria[Col] = SetContains[Elem, Col](value)
  }

  case class StringContains(value: String) extends SearchCriteria[String] with SimpleQueryStringSerialization[String] {
    override def check(value: String): Boolean = value.contains(this.value)
    override def getDeserializer: Deserializer[(String, String), StringContains] =
      SearchCriteriaDeserializer.stringContainsDeserializer
  }

  case class SeqContains[A, T <: Seq[A]](value: A) extends SearchCriteria[T] with SimpleQueryStringSerialization[A] {
    override def check(value: T): Boolean = value.contains(this.value)
    override def getDeserializer: Deserializer[(String, String), SeqContains[A, T]] =
      SearchCriteriaDeserializer.seqContainsDeserializer[A, T]
  }

  case class SetContains[A, T <: Set[A]](value: A) extends SearchCriteria[T] with SimpleQueryStringSerialization[A] {
    override def check(value: T): Boolean = value.contains(this.value)
    override def getDeserializer: Deserializer[(String, String), SetContains[A, T]] =
      SearchCriteriaDeserializer.setContainsDeserializer[A, T]
  }


  case class MatchRegEx(value: String) extends SearchCriteria[String] with SimpleQueryStringSerialization[String] {
    val regEx = value.r
    override def check(value: String): Boolean = regEx.findFirstIn(value).isDefined
    override def getDeserializer: Deserializer[(String, String), MatchRegEx] =
      SearchCriteriaDeserializer.matchRegExDeserializer
  }

  case class LessThan[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.lt(value, this.value)

    override def getDeserializer: Deserializer[(String, String), LessThan[N]] =
      SearchCriteriaDeserializer.lessThanDeserializer[N]
  }

  case class LessOrEqual[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
       with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.lteq(value, this.value)

    override def getDeserializer: Deserializer[(String, String), LessOrEqual[N]] =
      SearchCriteriaDeserializer.lessOrEqualDeserializer[N]
  }

  case class GreaterThan[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.gt(value, this.value)

    override def getDeserializer: Deserializer[(String, String), GreaterThan[N]] =
      SearchCriteriaDeserializer.greaterThanDeserializer[N]
  }

  case class GreaterOrEqual[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.gteq(value, this.value)

    override def getDeserializer: Deserializer[(String, String), GreaterOrEqual[N]] =
      SearchCriteriaDeserializer.greaterOrEqualDeserializer[N]
  }

  case class In[A](value: Seq[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = this.value.contains(value)

    override def toQueryString: Seq[(String, String)] = Seq("In" -> value.toString)
  }

  case class Between[A](from: A, to: A)(implicit ordering: Ordering[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = ordering.gt(value, from) && ordering.lt(value, to)
    override def toQueryString: Seq[(String, String)] = Seq(getClass.getSimpleName -> (from, to).toString)
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
    override def toQueryString: Seq[(String, String)] = Seq("NotEmptyString" -> "1")
  }

  case class NotEmptyCollection[T <: Traversable[_]]() extends SearchCriteria[T] {
    override def check(value: T): Boolean = value.nonEmpty
    override def toQueryString: Seq[(String, String)] = Seq("NotEmptyCollection" -> "1")
  }


}

