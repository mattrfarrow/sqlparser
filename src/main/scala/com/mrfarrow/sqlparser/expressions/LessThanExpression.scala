package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class LessThanExpression(left: Expression, right: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    Expression.assertSameTypes(left, right, thingToStrings)

    left.getType(thingToStrings) match {
      case ExpressionType.Integer => left.evaluateInt(thingToStrings, obj) < right.evaluateInt(thingToStrings, obj)
      case t       => throw new IllegalStateException("Unexpected type of left argument: "+left.getClass.getName)
    }
  }
}
