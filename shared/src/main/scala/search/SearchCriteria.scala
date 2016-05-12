package search

import model.Movie

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

  case class Criteria[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.check(value)
  }

  case class FieldCriteria[A](criteria: SearchCriteria[A]) extends SearchCriteria[A] {
    override def check(value: A): Boolean = criteria.check(value)
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

  case class Equal[A](value: A) extends SearchCriteria[A] {
    override def check(value: A): Boolean = this.value == value
  }

  /*case class Contains[A, T <: {def contains(a: A): Boolean}](value: A) extends SearchCriteria[T] {
    override def check(value: T): Boolean = value.contains(this.value)
  }*/

  trait ContainsInvoker[C, A] {
    def contains(a: A): SearchCriteria[C]
  }

  def Contains[C, A](value: A)(implicit containsInvoker: ContainsInvoker[C, A]): SearchCriteria[C] = {

  }

  implicit val stringContainsInvoker: ContainsInvoker[String, String] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  implicit val seqContainsInvoker: ContainsInvoker[Seq[A], A] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  implicit val stringContainsInvoker: ContainsInvoker[String, String] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  implicit val stringContainsInvoker: ContainsInvoker[String, String] = new ContainsInvoker[String, String] {
    override def contains(value: String): SearchCriteria[String] = StringContains(value)
  }

  trait ContainsCriteria[V, E, C[E]]

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



/*  case class Contains[A <: String](value: A) extends SearchCriteria[A] {
    override def check(value: A): Boolean = value.contains(this.value)
  }

  case class Contains[A <: Seq](value: A) extends SearchCriteria[A] {
    override def check(value: A): Boolean = value.contains(this.value)
  }*/

}

import SearchCriteria._

case class Title(override val criteria: SearchCriteria[String])
  extends FieldCriteria[Movie](SearchCriteria(m => criteria.check(m.title)))

object o {
  Iterable
  Set
  val d = "asd".contains("sdf")
  val a =
    Criteria[Movie](
      Title(Equal("Top gun"))
    )

}
