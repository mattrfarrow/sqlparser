package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class EqualsExpr(left: Expression, right: Expression) extends Expression {

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    Expression.assertSameTypes(left, right, thingToStrings)

    left.getType(thingToStrings) match {
      case ExpressionType.Integer => left.evaluateInt(thingToStrings, obj) == right.evaluateInt(thingToStrings, obj)
      case ExpressionType.Boolean => left.evaluateBool(thingToStrings, obj) == right.evaluateBool(thingToStrings, obj)
      case ExpressionType.String  => left.evaluateString(thingToStrings, obj) == right.evaluateString(thingToStrings, obj)
      case _       => throw new IllegalStateException()
    }
  }

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

}
