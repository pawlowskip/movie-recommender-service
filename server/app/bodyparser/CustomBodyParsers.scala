package bodyparser

import akka.util.ByteString
import play.api.libs.streams.Accumulator
import play.api.mvc._
import play.api.http.MimeTypes.JSON
import upickle.default._

import scala.concurrent.Future
import scala.util.Try

/**
  * Created by pp on 8/30/16.
  */
object CustomBodyParsers {
  def upickleJson[T] = BodyParser.apply[T]{ requestHeader =>
    requestHeader.contentType match {
      case Some(JSON) =>
        Accumulator.source[ByteString].mapFuture { source =>
          Future.fromTry(Try(read[T](source.toString())))
        }.map(Right.apply)
      case None =>
        Accumulator.done(Left(Results.UnsupportedMediaType))
    }
  }
}
