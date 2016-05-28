package search.core

import search.core.SearchCriteria.QueryStringParama

import scala.annotation.tailrec

/**
  * Created by pp on 5/7/16.
  */
sealed trait SearchCriteria[A] {
  def check(value: A): Boolean
  def toQueryString: Seq[QueryStringParama]
}

object SearchCriteria {

  type QueryStringParama = (String, String)
  type QueryString = Seq[QueryStringParama]

  def apply[A](f: A => Boolean, toQueryStringFun: => Seq[QueryStringParama] = Seq()) = new SearchCriteria[A] {
    override def check(value: A) = f(value)
    override def toQueryString: Seq[QueryStringParama] = toQueryStringFun
  }

/*  @throws[UnsupportedOperationException]
  def fromQueryString[A](queryString: Seq[QueryStringParama], queryStringName: String): Criteria[A] = {
    def analyzeFirst3andRest(queryStringParama: Seq[QueryStringParama]): Criteria[A] = {
      val (first3, rest) = queryStringParama.splitAt(3)
      first3 match {
        case Seq(("criteria", `queryStringName`), ("limit", "-1"), ("criteria", "-1")) =>
          val restAnalyzed = fromQueryString0(rest)
          restAnalyzed match {
            case Seq(single) => Criteria[A](single, queryStringName, None)
            case _ => throw new UnsupportedOperationException("Bad criteria body! " + rest)
          }

        case Seq(("criteria", `queryStringName`), ("limit", limit), ("criteria", page)) =>
          val restAnalyzed = fromQueryString0(rest)
          restAnalyzed match {
            case Seq(single) => Criteria[A](single, queryStringName, Some(SearchProps(limit.toInt, page.toInt)))
            case _ => throw new UnsupportedOperationException("Bad criteria body! " + rest)
          }
        case _ => throw new UnsupportedOperationException("Bad criteria header! " + first3)
      }
    }

    def fromQueryString0(queryStringParama: Seq[QueryStringParama]): Seq[Criteria[A]] =
      queryStringParama.head match {
        case ("And", number) =>
          val restAnalized = fromQueryString0(queryStringParama.tail)
          if (restAnalized.size != number) throw new UnsupportedOperationException("Bad number of inner criteria in And! " + restAnalized)
          ???
      }

    analyzeFirst3andRest(queryString)
  }*/

  case class SearchProps(limit: Int, page: Int)

  case class Criteria[A](criteria: SearchCriteria[A],
                         queryStringName: String = "",
                         props: Option[SearchProps] = None) extends SearchCriteria[A] {

    private val defaultLimit = 10

    override def check(value: A): Boolean = criteria.check(value)

    def limit(i: Int): Criteria[A] = this.copy(props = Some(SearchProps(i, 0)))

    def page(p: Int): Criteria[A] = props match {
      case None => this.copy(props = Some(SearchProps(defaultLimit, p)))
      case Some(SearchProps(l, _)) => this.copy(props = Some(SearchProps(l, p)))
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
  }

  trait SimpleQueryStringSerialization[A] {
    val value: A
    val paramaName: String = getClass.getSimpleName
    def toQueryString: Seq[QueryStringParama] = Seq(paramaName -> value.toString)
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
  }


  case class Equal[A](value: A) extends SearchCriteria[A] with SimpleQueryStringSerialization[A] {
    override def check(value: A): Boolean = this.value == value
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

  implicit def seqContainsInvoker[A, T <: Seq[A]]: ContainsInvoker[T, A] = new ContainsInvoker[T, A] {
    override def contains(value: A): SearchCriteria[T] = SeqContains[A, T](value)
  }

  implicit def setContainsInvoker[A, T <: Set[A]]: ContainsInvoker[T, A] = new ContainsInvoker[T, A] {
    override def contains(value: A): SearchCriteria[T] = SetContains[A, T](value)
  }

  case class StringContains(value: String) extends SearchCriteria[String] with SimpleQueryStringSerialization[String] {
    override def check(value: String): Boolean = value.contains(this.value)
  }

  case class SeqContains[A, T <: Seq[A]](value: A) extends SearchCriteria[T] with SimpleQueryStringSerialization[A] {
    override def check(value: T): Boolean = value.contains(this.value)
  }

  case class SetContains[A, T <: Set[A]](value: A) extends SearchCriteria[T] with SimpleQueryStringSerialization[A] {
    override def check(value: T): Boolean = value.contains(this.value)
  }


  case class MatchRegEx(value: String) extends SearchCriteria[String] with SimpleQueryStringSerialization[String] {
    val regEx = value.r
    override def check(value: String): Boolean = regEx.findFirstIn(value).isDefined
  }

  case class LessThan[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.lt(value, this.value)
  }

  case class LessOrEqual[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
       with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.lteq(value, this.value)
  }

  case class GreaterThan[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.gt(value, this.value)
  }

  case class GreaterOrEqual[N](value: N)(implicit ordering: Ordering[N])
    extends SearchCriteria[N]
      with SimpleQueryStringSerialization[N] {

    override def check(value: N): Boolean = ordering.gteq(value, this.value)
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

