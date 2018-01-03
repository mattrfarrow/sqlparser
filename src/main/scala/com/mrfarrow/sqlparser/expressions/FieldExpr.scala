package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class FieldExpr(name: String) extends Expression {
  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = thingToStrings.getBoolean(name, obj)
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = thingToStrings.getString(name, obj)
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = thingToStrings.getType(name)
  override def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = thingToStrings.getInt(name, obj)
}
