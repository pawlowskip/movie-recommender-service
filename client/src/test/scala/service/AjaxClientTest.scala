package service
import utest._
import utest.framework.{Test, Tree}
/**
  * Created by pp on 5/5/16.
  */
object AjaxClientTest extends TestSuite {
  val tests = this {
    'test1 {
      AjaxClient.test()
    }
  }
}
