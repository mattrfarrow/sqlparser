/**
  * Created by matt.farrow on 30/11/2017.
  */
trait ThingToStrings[T] {
  def getType(name: String): ExpressionType

  def getString(name: String, obj: T): String

  def getBoolean(name: String, obj: T): Boolean
}
