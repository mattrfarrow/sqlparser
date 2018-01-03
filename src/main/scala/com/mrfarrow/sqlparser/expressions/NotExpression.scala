package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class NotExpression(expr: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = expr.getType(thingToStrings) match {
    case ExpressionType.Boolean  => !expr.evaluateBool(thingToStrings, obj)
    case _       => throw new IllegalStateException("Not can only be applied to boolean expressions")
  }
}
