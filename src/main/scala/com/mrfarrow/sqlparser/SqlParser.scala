package com.mrfarrow.sqlparser



import scala.util.Try
import scala.util.parsing.combinator._


object SqlParser {

  def parse(sql: String, thingToStrings: ThingToStrings[_]): Try[SqlQuery] = {
    new SqlParser(thingToStrings).parse(sql)
  }

}

case class SqlQuery(fields: Array[Expression], from: Option[String], where: Option[Expression]) {
  override def toString: String = "SELECT " + fields.mkString(",") + " FROM " + from + where.map(e => " WHERE "+e)
}

object ExpressionUtil {
  def assertSameTypes(a: Expression, b: Expression, thingToStrings: ThingToStrings[_]) {
    if (a.getType (thingToStrings) != b.getType (thingToStrings) ) {
      throw new Exception ("Left side type is " + a.getType (thingToStrings) + " right side is " + b.getType (thingToStrings) )
    }
  }
}

abstract class Expression {
  def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType
  def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = throw new UnsupportedOperationException
  def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = throw new UnsupportedOperationException("Not supported by "+getClass.getName)
  def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = throw new UnsupportedOperationException
}

case class FieldExpr(name: String) extends Expression {
  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = thingToStrings.getBoolean(name, obj)
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = thingToStrings.getString(name, obj)
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = thingToStrings.getType(name)
  override def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = thingToStrings.getInt(name, obj)
}

case class From(s: String)
case class Where(expr: Expression)

class LengthExpr(expr: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Integer

  override def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = expr.getType(thingToStrings) match {
    case ExpressionType.String  => expr.evaluateString(thingToStrings, obj).length
    case _       => throw new IllegalStateException()
  }

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = evaluateInt(thingToStrings, obj).toString
}

case class EqualsExpr(left: Expression, right: Expression) extends Expression {

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    ExpressionUtil.assertSameTypes(left, right, thingToStrings)

    left.getType(thingToStrings) match {
      case ExpressionType.Integer => left.evaluateInt(thingToStrings, obj) == right.evaluateInt(thingToStrings, obj)
      case ExpressionType.Boolean => left.evaluateBool(thingToStrings, obj) == right.evaluateBool(thingToStrings, obj)
      case ExpressionType.String  => left.evaluateString(thingToStrings, obj) == right.evaluateString(thingToStrings, obj)
      case _       => throw new IllegalStateException()
    }
  }

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

}

case class GreaterThanExpression(left: Expression, right: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    ExpressionUtil.assertSameTypes(left, right, thingToStrings)

    left.getType(thingToStrings) match {
      case ExpressionType.Integer => left.evaluateInt(thingToStrings, obj) > right.evaluateInt(thingToStrings, obj)
      case t       => throw new IllegalStateException("Unexpected type of left argument: "+left.getClass.getName)
    }
  }
}

case class LessThanExpression(left: Expression, right: Expression) extends Expression {
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Boolean

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    ExpressionUtil.assertSameTypes(left, right, thingToStrings)

    left.getType(thingToStrings) match {
      case ExpressionType.Integer => left.evaluateInt(thingToStrings, obj) < right.evaluateInt(thingToStrings, obj)
      case t       => throw new IllegalStateException("Unexpected type of left argument: "+left.getClass.getName)
    }
  }
}

case class LikeExpr(left: Expression, toMatch: LiteralStringExpr) extends Expression {

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    val regex = ParserRegex.createRegexFromGlob(toMatch.string)
    val leftThing: String = left.evaluateString(thingToStrings, obj)
    leftThing.matches(regex)
  }

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = ExpressionType.Boolean
}

case class LiteralStringExpr(string: String) extends Expression {
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = string

  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.String
}

case class LiteralIntExpr(int: Int) extends Expression {
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = int.toString

  override def evaluateInt[T](thingToStrings: ThingToStrings[T], obj: T): Int = int

  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.Integer
}

class SqlParser(thingToStrings: ThingToStrings[_]) extends RegexParsers {
  def parse(sql: String): Try[SqlQuery] = parse(phrase(selectFrom), sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg ))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }

  private def expression: Parser[_ <: Expression] =
    likeExpr | comparitiveExpr | lengthExpr | field | literalExpression | bracketedExpression
  private def standaloneExpression: Parser[_ <: Expression] =
    literalExpression | lengthExpr | field | bracketedExpression  | comparitiveExpr
  private def bracketedExpression: Parser[Expression] =
    ("(" ~ expression ~ ")") ^^ {case b ~ ex ~ b2 => ex}

  private def equalsExpression: Parser[EqualsExpr] =
    standaloneExpression ~ "=" ~ standaloneExpression ^^ { case left ~ eq ~ right  => EqualsExpr(left, right) }

  private def greaterThanExpr: Parser[GreaterThanExpression] =
    standaloneExpression~ ">" ~ standaloneExpression ^^ { case left ~ gt ~ right  => GreaterThanExpression(left, right) }

  private def lessThanExpr: Parser[LessThanExpression] =
    standaloneExpression~ "<" ~ standaloneExpression ^^ { case left ~ gt ~ right  => LessThanExpression(left, right) }

  private def likeExpr: Parser[LikeExpr] ={
    standaloneExpression ~ "like" ~ literalStringExpr ^^ { case left ~ like ~ right  => LikeExpr(left, right) }
  }

  private def comparitiveExpr = equalsExpression | likeExpr | greaterThanExpr | lessThanExpr

  private def lengthExpr: Parser[LengthExpr] ={
    "length" ~ bracketedExpression ^^ { case length ~ bracketedExpr  => new LengthExpr(bracketedExpr) }
  }

  private def field: Parser[Expression]   = """[a-z]+""".r       ^^ { s => FieldExpr(s) }
  private def fields: Parser[Array[Expression]]  = expression ~ opt("," ~ fields) ^^ {
    case f ~ Some(comma ~ fs) => Array(f) ++ fs
    case f ~ None => Array(f)}

  private def literalStringExpr: Parser[LiteralStringExpr] =
     "'" ~ """[a-z*]+""".r ~ "'" ^^ { case q1 ~ s ~ q2 => LiteralStringExpr(s) }

  private def literalIntExpr: Parser[LiteralIntExpr] =
    """(0|[1-9]\d*)""".r ^^ {s => LiteralIntExpr(s.toInt) }

  private def literalExpression: Parser[Expression] = literalIntExpr | literalStringExpr

  private def directoryPath: Parser[String] = """[/.A-Za-z]+""".r

  private def from: Parser[String] = "from" ~ directoryPath ^^ { case from ~ place => place }
  private def where: Parser[Expression] = "where" ~ expression ^^ {case whereEx ~ expr => expr}

  private def selectFrom: Parser[SqlQuery] = "select" ~ fields ~ opt(from) ~ opt(where) ^^ {
    case select ~ fs ~ from  ~ where => SqlQuery(fs, from, where)
  }

  private def word: Parser[String] = """[a-z]+""".r
}
