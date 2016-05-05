package service
import org.scalajs.dom
import upickle.default._
import upickle._
import autowire._

import scala.concurrent.duration._
import scala.concurrent.Future
import dom.html
import dom.ext._
import model.Movie
import scala.collection.immutable.TreeMap
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Created by pp on 5/4/16.
  */
object ajax{

/*  case class AjaxCallResponse(status: Int)
  case class AjaxCallSuccessResponse[A](resp: A, status: Int)
  case class AjaxCallTimeoutResponse(cause: , status: Int)
  case class AjaxCallFailureResponse(cause: , status: Int)*/

  case class MimeType(str: String)

  object MimeType {
    val notSpecified = MimeType("")
    val html = MimeType("text/html")
    val xml = MimeType("application/xml")
    val json = MimeType("application/json")
  }

  object AjaxRequest {
    private val igoreCaseStringOrdering = new Ordering[String] {
      override def compare(x: String, y: String): Int = {
        x.compareToIgnoreCase(y)
      }
    }
  }

  case class AjaxRequest(url: String = "",
                         headers: Map[String, String] = TreeMap[String, String]()(AjaxRequest.igoreCaseStringOrdering),
                         queryString: Map[String, String] = TreeMap[String, String]()(AjaxRequest.igoreCaseStringOrdering),
                         timeout: Duration = 0.millis,
                         withCredentials: Boolean = false,
                         responseType: MimeType = MimeType.notSpecified) {

    def withHeaders(headers: (String, String)*): AjaxRequest = copy(headers = this.headers ++ headers)
    def withHeaders(headers: Map[String, String]): AjaxRequest = withHeaders(headers.toSeq: _*)
    def withTimeout(timeout: Duration): AjaxRequest = copy(timeout = timeout)
    def withQueryString(params: (String, String)*): AjaxRequest = copy(queryString = this.queryString ++ queryString)
    def withQueryString(params: Map[String, String]): AjaxRequest = withQueryString(params.toSeq: _*)
    def withUrl(url: String): AjaxRequest = copy(url = url)
    def withCredentials(b: Boolean): AjaxRequest = copy(withCredentials = b)
    def withResponseType(responseType: MimeType): AjaxRequest = copy(responseType = responseType)


  }

  def url(url: String): AjaxRequest = AjaxRequest().withUrl(url)



  def get[A](url: String,
             timeout: Duration = 0.millis,
             headers: Map[String, String] = Map.empty,
             withCredentials: Boolean = false,
             responseType: String = ""): Future[A] = {

    Ajax.get(url
    ).map(res => read[A](res.responseText))
  }





}
