package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

class LengthExpr(expr: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Integer

  override def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = expr.getType(thingToStrings) match {
    case ExpressionType.String  => expr.evaluateString(thingToStrings, obj).length
    case _       => throw new IllegalStateException()
  }

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = evaluateInt(thingToStrings, obj).toString
}
