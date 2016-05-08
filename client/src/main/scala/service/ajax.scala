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
import org.scalajs.dom.ext.Ajax.InputData

import scala.collection.immutable.TreeMap
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Created by pp on 5/4/16.
  */
object ajax {

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
    private val ignoreCaseStringOrdering = new Ordering[String] {
      override def compare(x: String, y: String): Int = {
        x.compareToIgnoreCase(y)
      }
    }
  }

  case class AjaxRequest(url: String = "",
                         inputData: Option[String] = None,
                         headers: Map[String, String] = TreeMap[String, String]()(AjaxRequest.ignoreCaseStringOrdering),
                         queryString: Map[String, String] = TreeMap[String, String]()(AjaxRequest.ignoreCaseStringOrdering),
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

    def withInputData(data: String): AjaxRequest = copy(inputData = Some(data))

    def get[A](implicit reader: upickle.default.Reader[A]): Future[A] = call("GET")(reader)

    def post[A](contentType: MimeType)(implicit reader: upickle.default.Reader[A]): Future[A] =
      withHeaders("Content-Type" -> contentType.str).call("POST")(reader)

    def delete[A](implicit reader: upickle.default.Reader[A]): Future[A] = call("DELETE")(reader)

    def put[A](contentType: MimeType)(implicit reader: upickle.default.Reader[A]): Future[A] =
      withHeaders("Content-Type" -> contentType.str).call("PUT")(reader)

    private def call[A](method: String)(implicit reader: upickle.default.Reader[A]): Future[A] =
      Ajax.apply(
        method = method,
        url = url,
        data = InputData.str2ajax(inputData.orNull),
        headers = headers,
        timeout = timeout.toMillis.toInt,
        withCredentials = withCredentials,
        responseType = responseType.str
      ).map(resp => read(resp.responseText)(reader))

  }

  def url(url: String): AjaxRequest = AjaxRequest().withUrl(url)

}
