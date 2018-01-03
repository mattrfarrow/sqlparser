package com.mrfarrow.sqlparser.expressions

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

abstract class Expression {
  def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType
  def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = throw new UnsupportedOperationException
  def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = throw new UnsupportedOperationException("Not supported by "+getClass.getName)
  def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = throw new UnsupportedOperationException
}

object Expression {
  def assertSameTypes(a: Expression, b: Expression, thingToStrings: ThingToStrings[_]) {
    if (a.getType (thingToStrings) != b.getType (thingToStrings) ) {
      throw new Exception ("Left side type is " + a.getType (thingToStrings) + " right side is " + b.getType (thingToStrings) )
    }
  }
}

