package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class LiteralIntExpr(int: Int) extends Expression {
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = int.toString

  override def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = int

  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Integer
}
