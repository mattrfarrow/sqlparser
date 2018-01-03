package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ParserRegex, ThingToStrings}

case class LikeExpr(left: Expression, toMatch: LiteralStringExpr) extends Expression {

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    val regex = ParserRegex.createRegexFromGlob(toMatch.string)
    val leftThing: String = left.evaluateString(thingToStrings, obj)
    leftThing.matches(regex)
  }

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = ExpressionType.Boolean
}
