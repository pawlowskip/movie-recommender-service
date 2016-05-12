package search.core

/**
  * Created by pp on 5/7/16.
  */
trait SearchCriteria[A] {
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

  def field[C, F](f: C => F): SearchCriteria[F] => SearchCriteria[C] =
    (c: SearchCriteria[F]) => new SearchCriteria[C] {
    override def check(value: C): Boolean = c.check(f(value))
  }



  implicit def toFilterExecutor[A](criteria: Criteria[A]): FilterExecutor[A] = new FilterExecutor(criteria)

  class FilterExecutor[A](criteria: Criteria[A]) {
    def filter[C <: Iterable[A]](collection: C): Iterable[A] = criteria match {
      case Criteria(searchCriteria, None) => collection.filter(a => searchCriteria.check(a))
      case Criteria(searchCriteria, Some(SearchProps(limit, page))) =>
        collection.filter(a => searchCriteria.check(a)).slice((page + 1) * limit, page * limit)
    }

  }


  case class And[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.forall(_.check(value))
  }

  object And {
    def apply[A](criteria: SearchCriteria[A]*) = And(criteria)
  }

  case class Or[A](criteria: Seq[SearchCriteria[A]]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.exists(_.check(value))
  }

  object Or {
    def apply[A](criteria: SearchCriteria[A]*) = Or(criteria)
  }

  case class Not[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = !criteria.check(value)
  }

  object Not {
    def apply[A](criteria: SearchCriteria[A]) = Not(criteria)
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

  implicit def seqContainsInvoker[A]: ContainsInvoker[Seq[A], A] = new ContainsInvoker[Seq[A], A] {
    override def contains(value: A): SearchCriteria[Seq[A]] = SeqContains(value)
  }

  implicit def seqContainsInvoker[A]: ContainsInvoker[Set[A], A] = new ContainsInvoker[Set[A], A] {
    override def contains(value: A): SearchCriteria[Set[A]] = SetContains(value)
  }

  implicit def seqContainsInvoker[K, V]: ContainsInvoker[Map[K, V], K] = new ContainsInvoker[Map[K, V], K] {
    override def contains(value: K): SearchCriteria[Map[K, V]] = MapContains(value)
  }

  case class StringContains(value: String) extends SearchCriteria[String] {
    override def check(value: String): Boolean = value.contains(this.value)
  }

  case class SeqContains[A](value: A) extends SearchCriteria[Seq[A]] {
    override def check(value: Seq[A]): Boolean = value.contains(this.value)
  }

  case class SetContains[A](value: A) extends SearchCriteria[Set[A]] {
    override def check(value: Set[A]): Boolean = value.contains(this.value)
  }

  case class MapContains[K, V](value: K) extends SearchCriteria[Map[K, V]] {
    override def check(value: Map[K, V]): Boolean = value.contains(this.value)
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

}

