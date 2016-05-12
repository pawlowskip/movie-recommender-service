package search.core
import utest._
import utest.framework.{Test, Tree}
/**
  * Created by pp on 5/12/16.
  */
object SearchCriteriaTest extends TestSuite {
  import SearchCriteria._

  val tests = this {
    /* Helper data classes for tests*/
    case class Rectangle(a: Double, b: Double)

    "Test[1] - SearchCriteria companion : apply method" - {

      val criteria = SearchCriteria.apply((i: Int) => i < 3)

      "should return instance of SearchCriteria[Int]" - {
        assertMatch(criteria){
          case res: SearchCriteria[Int] =>
        }
      }

      "should check if x < 3" - {
        * - {criteria.check(1) == true}
        * - {criteria.check(3) == false}
        * - {criteria.check(45) == false}
      }

    }

    "Test[2] - Equal SearchCriteria" - {
      * - {Equal("Ala").check("Ala") == true}
      * - {Equal("Ala").check("ala") == false}
      * - {Equal(12).check(1) == false}
      * - {Equal(Some(1)).check(Some(-1)) == false}
      * - {Equal(Some(1): Option[Int]).check(None) == false}
    }

    "Test[3] - Logical SearchCriteria" - {

      val tupleFirstOneCriteria: SearchCriteria[(Int, Int)] = SearchCriteria(t => t._1 == 1)
      val tupleSecondTwoCriteria: SearchCriteria[(Int, Int)] = SearchCriteria(t => t._2 == 2)

      "And criteria" - {
        val criteria = Criteria[(Int, Int)](
          And(
            tupleFirstOneCriteria,
            tupleSecondTwoCriteria
          )
        )

        * - {criteria.check((1, 2)) == true}
        * - {criteria.check((1, 3)) == false}
        * - {criteria.check((3, 2)) == false}
        * - {criteria.check((-1, 22)) == false}
      }

      "Or criteria" - {
        val criteria = Criteria[(Int, Int)](
          Or(
            tupleFirstOneCriteria,
            tupleSecondTwoCriteria
          )
        )

        * - {criteria.check((1, 2)) == true}
        * - {criteria.check((1, 3)) == true}
        * - {criteria.check((3, 2)) == true}
        * - {criteria.check((-1, 22)) == false}
      }

      "Not criteria" - {
        val criteria = Criteria[(Int, Int)](
          Not(
            tupleFirstOneCriteria
          )
        )

        * - {criteria.check((1, 2)) == false}
        * - {criteria.check((4, 3)) == true}
      }

    }

  }
}
