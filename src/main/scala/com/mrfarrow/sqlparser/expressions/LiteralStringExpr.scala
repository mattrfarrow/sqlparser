package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class LiteralStringExpr(string: String) extends Expression {
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = string

  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.String
}
