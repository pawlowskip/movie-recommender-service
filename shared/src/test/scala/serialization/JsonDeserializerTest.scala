package serialization
import utest._
import Deserializer.input
import upickle.default._
/**
  * Created by pp on 5/27/16.
  */
object JsonDeserializerTest extends TestSuite {
  case class A(a: Int, o: Option[String])

  val tests = this {
    "Test [1] - json deserializer" - {
      val a = A(1, Some("aa"))
      val serialized = Serializer.json[A].serialize(a)
      val result = DeserializationDefaults.json[A].deserialize(input(serialized))
      assert(a == result.asInstanceOf[Ok[A, String]].result)
    }
  }

}
