package search.core
import utest._
/**
  * Created by pp on 5/12/16.
  */
object SearchCriteriaTest extends TestSuite {
  import SearchCriteria._

  val tests = this {

    "Test[1] - SearchCriteria companion : apply method" - {

      val criteria = SearchCriteria.apply((i: Int) => i < 3)

      "should return instance of SearchCriteria[Int]" - {
        assert(criteria.isInstanceOf[SearchCriteria[Int]])
      }

      "should check if x < 3" - {
        * - {assert(criteria.check(1) == true)}
        * - {assert(criteria.check(3) == false)}
        * - {assert(criteria.check(45) == false)}
      }

    }

    "Test[2] - Equal SearchCriteria" - {
      * - {assert(Equal("Ala").check("Ala") == true)}
      * - {assert(Equal("Ala").check("ala") == false)}
      * - {assert(Equal(12).check(1) == false)}
      * - {assert(Equal(Some(1)).check(Some(-1)) == false)}
      * - {assert(Equal(Some(1): Option[Int]).check(None) == false)}
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

        * - {assert(criteria.check((1, 2)) == true)}
        * - {assert(criteria.check((1, 3)) == false)}
        * - {assert(criteria.check((3, 2)) == false)}
        * - {assert(criteria.check((-1, 22)) == false)}
      }

      "Or criteria" - {
        val criteria = Criteria[(Int, Int)](
          Or(
            tupleFirstOneCriteria,
            tupleSecondTwoCriteria
          )
        )

        * - {assert(criteria.check((1, 2)) == true)}
        * - {assert(criteria.check((1, 3)) == true)}
        * - {assert(criteria.check((3, 2)) == true)}
        * - {assert(criteria.check((-1, 22)) == false)}
      }

      "Not criteria" - {
        val criteria = Criteria[(Int, Int)](
          Not(
            tupleFirstOneCriteria
          )
        )

        * - {assert(criteria.check((1, 2)) == false)}
        * - {assert(criteria.check((4, 3)) == true)}
      }

    }

    "Test[4] - Contains Criteria" - {

      "String Contains" - {
        val criteria = Criteria[String](Contains("abcd"))

        * - {assert(criteria.check("abcd") == true)}
        * - {assert(criteria.check("Abcd") == false)}
        * - {assert(criteria.check(" rabcdfg") == true)}
      }

      "Seq Contains" - {
        val criteria = Criteria[List[Int]](
          Contains(1)
        )

        * - {assert(criteria.check(List(1)) == true)}
        * - {assert(criteria.check(List()) == false)}
        * - {assert(criteria.check(List(1, 2, 3)) == true)}
        * - {assert(criteria.check(List(0, 2, 1)) == true)}
      }

      "Set Contains" - {
        val criteria = Criteria[Set[Int]](
          Contains(1)
        )

        * - {assert(criteria.check(Set(1)) == true)}
        * - {assert(criteria.check(Set()) == false)}
        * - {assert(criteria.check(Set(1, 2, 3)) == true)}
        * - {assert(criteria.check(Set(0, 2, 1)) == true)}
      }

    }

    "Test[5] - MatchRegEx Criteria" - {
      val criteria = Criteria[String](
        MatchRegEx("^ala.*makota$")
      )

      * - {assert(criteria.check("alamakota") == true)}
      * - {assert(criteria.check("alaasdkgalksugdlmakota") == true)}
      * - {assert(criteria.check(" alamakota") == false)}
      * - {assert(criteria.check("alamakota ") == false)}
    }

    "Test[6] - Comparing Criteria" - {

      "LessThan" - {
        val criteria = Criteria[Double](LessThan(0.0))

        * - {assert(criteria.check(0.0) == false)}
        * - {assert(criteria.check(-1.0) == true)}
        * - {assert(criteria.check(1.0) == false)}
      }

      "LessOrEqual" - {
        val criteria = Criteria[Double](LessOrEqual(0.0))

        * - {assert(criteria.check(0.0) == true)}
        * - {assert(criteria.check(-1.0) == true)}
        * - {assert(criteria.check(1.0) == false)}
      }

      "GreaterThan" - {
        val criteria = Criteria[Double](GreaterThan(0.0))

        * - {assert(criteria.check(0.0) == false)}
        * - {assert(criteria.check(-1.0) == false)}
        * - {assert(criteria.check(1.0) == true)}
      }

      "GreaterOrEqual" - {
        val criteria = Criteria[Double](GreaterOrEqual(0.0))

        * - {assert(criteria.check(0.0) == true)}
        * - {assert(criteria.check(-1.0) == false)}
        * - {assert(criteria.check(1.0) == true)}
      }

    }

    "Test[7] - Others Criteria" - {

      "In Criteria" - {
        val criteria = Criteria[Int](In(List(1, 2, 3)))

        * - {assert(criteria.check(1) == true)}
        * - {assert(criteria.check(5) == false)}
      }

      "Between Criteria" - {
        val criteria = Criteria[Int](Between(1, 10))

        * - {assert(criteria.check(1) == false)}
        * - {assert(criteria.check(5) == true)}
        * - {assert(criteria.check(10) == false)}
      }

      "IsEmpty String Criteria" - {
        val criteria = Criteria[String](IsEmpty)

        * - {assert(criteria.check("") == true)}
        * - {assert(criteria.check(" ") == false)}
        * - {assert(criteria.check("asd") == false)}
      }

      "IsEmpty Collection Criteria" - {
        val criteria = Criteria[List[Int]](IsEmpty)

        * - {assert(criteria.check(Nil) == true)}
        * - {assert(criteria.check(List(1)) == false)}
        * - {assert(criteria.check(List(1, 2)) == false)}
      }

      "NotEmpty String Criteria" - {
        val criteria = Criteria[String](NotEmpty)

        * - {assert(criteria.check("") == false)}
        * - {assert(criteria.check(" ") == true)}
        * - {assert(criteria.check("asd") == true)}
      }

      "NotEmpty Collection Criteria" - {
        val criteria = Criteria[List[Int]](NotEmpty)

        * - {assert(criteria.check(Nil) == false)}
        * - {assert(criteria.check(List(1)) == true)}
        * - {assert(criteria.check(List(1, 2)) == true)}
      }

    }

    "Test[8] - filter on Collection" - {
      val strings = List("Ala", "ma", "kota", "a", "kot", "ma", "alę")
      val criteria = Criteria[String](MatchRegEx("kot.*"))

      * - {assert(criteria.filter(strings) == List("kota", "kot"))}
    }

    "Test[9] - Criteria Props" - {
      val numbers = 1 to 100
      val criteria = Criteria[Int](GreaterThan(30)).limit(10).page(0)
      val criteria2 = criteria.page(1)
      val filtered1 = criteria.filter(numbers)
      val firstPage = 31 to 40
      val filtered2 = criteria2.filter(numbers)
      val secondPage = 41 to 50

      * - {assert(filtered1 == firstPage)}
      * - {assert(filtered2 == secondPage)}

    }


  }
}
