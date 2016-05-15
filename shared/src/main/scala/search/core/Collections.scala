package search.core

/**
  * Created by pp on 5/14/16.
  */
object Collections {
  import upickle.default._

  sealed trait Criteria
  case class GreaterThan(value: Int) extends Criteria

  sealed trait TupleFieldCriteria extends Criteria
  case class First(criteria: Criteria) extends TupleFieldCriteria
  case class Second(criteria: Criteria) extends TupleFieldCriteria

  case class TupleCriteria(criteria: Criteria) extends Criteria

  //case class And(criteria: Seq[Criteria])
  //case class Or(criteria: Seq[Criteria])
  //case class Not(criteria: Criteria)

  case class X()
  case class Y(name: String, next: X)

  case class Thing(myFieldA: Int, myFieldB: String)
  case class Big(i: Int, b: Boolean, str: String, c: Char, t: Thing)


  sealed trait S
  case class Something(i: Int, s: String) extends S
  case class SetOfSomething(s: Set[Something],t: String) extends S

  sealed class M[I, M]
  object M {
    implicit val int2String = new M[Int, String]
    implicit val something2Set = new M[Something, SetOfSomething]
  }

  type Wr[T] = upickle.default.Writer[T]

  def doSomething[K, V](k: K, v: V)(implicit ev: M[K, V], wr: Wr[V]): upickle.Js.Value ={
    upickle.default.writeJs(v)
  }
}
