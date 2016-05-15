package search.core

/**
  * Created by pp on 5/7/16.
  */
sealed trait SearchCriteria[A] {
  def check(value: A): Boolean
}

object SearchCriteria {

  def apply[A](f: A => Boolean) = new SearchCriteria[A] {
    override def check(value: A) = f(value)
  }

  case class SearchProps(limit: Int, page: Int)

  case class Criteria[A](criteria: SearchCriteria[A],
                         props: Option[SearchProps] = None) extends SearchCriteria[A] {

    private val defaultLimit = 10

    override def check(value: A): Boolean = criteria.check(value)
    def limit(i: Int): Criteria[A] = this.copy(props = Some(SearchProps(i, 0)))
    def page(p: Int): Criteria[A] = props match {
      case None => this.copy(props = Some(SearchProps(0, p)))
      case Some(SearchProps(l, _)) => this.copy(props = Some(SearchProps(l, p)))
      case _ => this.copy(props = Some(SearchProps(defaultLimit, p)))
    }
  }

  abstract class Field[F, C](f: C => F) extends SearchCriteria[C] {
    val criteria: SearchCriteria[F]
    override def check(value: C): Boolean = criteria.check(f(value))
  }

  /*def field[C, F](f: C => F): SearchCriteria[F] => SearchCriteria[C] =
    (c: SearchCriteria[F]) => new SearchCriteria[C] {
    override def check(value: C): Boolean = c.check(f(value))
  }*/

  implicit def toFilterExecutor[A](criteria: Criteria[A]): FilterExecutor[A] = new FilterExecutor(criteria)

  class FilterExecutor[A](criteria: Criteria[A]) {
    def filter[C <: Iterable[A]](collection: C): Iterable[A] = criteria match {
      case Criteria(searchCriteria, None) => collection.filter(a => searchCriteria.check(a))
      case Criteria(searchCriteria, Some(SearchProps(limit, page))) =>
        collection.filter(a => searchCriteria.check(a)).slice(page * limit, (page + 1) * limit)
    }
  }


  case class And[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.forall(_.check(value))
  }

  object And {
    def apply[A](criteria: SearchCriteria[A]*): SearchCriteria[A] = And(criteria)
  }

  case class Or[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.exists(_.check(value))
  }

  object Or {
    def apply[A](criteria: SearchCriteria[A]*): SearchCriteria[A] = Or(criteria)
  }

  case class Not[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = !criteria.check(value)
  }


  case class Equal[A](value: A) extends SearchCriteria[A] {
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

  case class StringContains(value: String) extends SearchCriteria[String] {
    override def check(value: String): Boolean = value.contains(this.value)
  }

  case class SeqContains[A, T <: Seq[A]](value: A) extends SearchCriteria[T] {
    override def check(value: T): Boolean = value.contains(this.value)
  }

  case class SetContains[A, T <: Set[A]](value: A) extends SearchCriteria[T] {
    override def check(value: T): Boolean = value.contains(this.value)
  }


  case class MatchRegEx(regexStr: String) extends SearchCriteria[String] {
    val regEx = regexStr.r
    override def check(value: String): Boolean = regEx.findFirstIn(value).isDefined
  }

  case class LessThan[N](value: N)(implicit ordering: Ordering[N]) extends SearchCriteria[N] {
    override def check(value: N): Boolean = ordering.lt(value, this.value)
  }

  case class LessOrEqual[N](value: N)(implicit ordering: Ordering[N])  extends SearchCriteria[N] {
    override def check(value: N): Boolean = ordering.lteq(value, this.value)
  }

  case class GreaterThan[N](value: N)(implicit ordering: Ordering[N]) extends SearchCriteria[N] {
    override def check(value: N): Boolean = ordering.gt(value, this.value)
  }

  case class GreaterOrEqual[N](value: N)(implicit ordering: Ordering[N])  extends SearchCriteria[N] {
    override def check(value: N): Boolean = ordering.gteq(value, this.value)
  }

  case class In[A](values: Seq[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = values.contains(value)
  }

  case class Between[A](from: A, to: A)(implicit ordering: Ordering[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = ordering.gt(value, from) && ordering.lt(value, to)
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
  }

  case class NotEmptyCollection[T <: Traversable[_]]() extends SearchCriteria[T] {
    override def check(value: T): Boolean = value.nonEmpty
  }


}

