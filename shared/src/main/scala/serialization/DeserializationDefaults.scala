package serialization

import serialization.Deserializer.DeserializerBuilder
import upickle.default._

/**
  * Created by pp on 5/27/16.
  */
object DeserializationDefaults {
  val int = DeserializerBuilder.transform[String, Int](_.toInt)
  val double = DeserializerBuilder.transform[String, Double](_.toDouble)
  val long = DeserializerBuilder.transform[String, Long](_.toLong)
  val string = DeserializerBuilder.transform[String, String](s => s)

  def json[A](implicit reader: Reader[A]) = DeserializerBuilder.transform[String, A](read[A](_))
}
