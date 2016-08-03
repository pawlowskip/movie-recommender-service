package serialization

/**
  * Created by pp on 7/30/16.
  */
trait TokenConverter[Token, Value] {
  def convertTo(token: Token): Value
  def convertFrom(value: Value): Token
}
