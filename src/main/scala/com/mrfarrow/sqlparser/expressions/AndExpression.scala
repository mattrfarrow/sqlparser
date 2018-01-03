package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

case class AndExpression(expr1: Expression, expr2: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = (expr1.getType(thingToStrings), expr2.getType(thingToStrings)) match {
    case (ExpressionType.Boolean, ExpressionType.Boolean)  => expr1.evaluateBool(thingToStrings, obj) && expr2.evaluateBool(thingToStrings, obj)
    case _                                                 => throw new IllegalStateException("Not can only be applied to boolean expressions")
  }
}
