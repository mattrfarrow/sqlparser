package com.mrfarrow.sqlparser

import scala.util.Try
import scala.util.parsing.combinator._


case class SqlQuery(fields: Array[FieldExpr], from: String, where: Option[Expression]) {
  override def toString: String = "SELECT " + fields.mkString(",") + " FROM " + from + where.map(e => " WHERE "+e)
}

trait Expression {
  def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType
  def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean
  def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String
}

case class FieldExpr(name: String) extends Expression {
  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = thingToStrings.getBoolean(name, obj)
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = thingToStrings.getString(name, obj)
  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = thingToStrings.getType(name)
}

case class EqualsExpr(left: Expression, right: Expression) extends Expression {

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    if(left.getType(thingToStrings) != right.getType(thingToStrings)) {
      throw new Exception("Left side type is " + left.getType(thingToStrings) + " right side is " + right.getType(thingToStrings))
    }

    left.getType(thingToStrings) match {
      case ExpressionType.Boolean => left.evaluateBool(thingToStrings, obj) == right.evaluateBool(thingToStrings, obj)
      case ExpressionType.String  => left.evaluateString(thingToStrings, obj) == right.evaluateString(thingToStrings, obj)
      case _       => throw new IllegalStateException()
    }
  }

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = throw new UnsupportedOperationException

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = ExpressionType.Boolean
}

case class LikeExpr(left: Expression, toMatch: LiteralStringExpr, thingToStrings: ThingToStrings[_]) extends Expression {

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    val regex = createRegexFromGlob(toMatch.string)
    val leftThing: String = left.evaluateString(thingToStrings, obj)
    leftThing.matches(regex)
  }

  private def createRegexFromGlob(glob: String) = {
    val out = new StringBuilder("^")
    var i = 0
    while ( {
      i < glob.length
    }) {
      val c = glob.charAt(i)
      c match {
        case '*' =>
          out.append(".*")
        case '?' =>
          out.append('.')
        case '.' =>
          out.append("\\.")
        case '\\' =>
          out.append("\\\\")
        case _ =>
          out.append(c)
      }

      {
        i += 1; i
      }
    }
    out.append('$')
    out.toString
  }

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T) = throw new UnsupportedOperationException

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = ExpressionType.Boolean
}

case class LiteralStringExpr(string: String) extends Expression {
  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = throw new UnsupportedOperationException

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = string

  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.String
}
case class Where(expr: Expression)


class SqlParser(thingToStrings: ThingToStrings[_]) extends RegexParsers {

  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def expression: Parser[_ <: Expression] =   likeExpr | equalsExpression | field | literalStringExpr

  def bracketedExpression: Parser[Expression] = ("(" ~ expression ~ ")") ^^ {case b ~ ex ~ b2 => ex}
  def standaloneExpression: Parser[_ <: Expression] = literalStringExpr | field | bracketedExpression

  def equalsExpression: Parser[EqualsExpr] ={
    standaloneExpression ~ "=" ~ standaloneExpression ^^ { case left ~ eq ~ right  => EqualsExpr(left, right) }
  }

  def likeExpr: Parser[LikeExpr] ={
    standaloneExpression ~ "like" ~ literalStringExpr ^^ { case left ~ like ~ right  => LikeExpr(left, right, thingToStrings) }
  }

  def field: Parser[FieldExpr]   = """[a-z]+""".r       ^^ { s => FieldExpr(s) }
  def fields: Parser[Array[FieldExpr]]  = field ~ opt("," ~ fields) ^^ {
    case f ~ Some(comma ~ fs) => Array(f) ++ fs
    case f ~ None => Array(f)}

  def literalStringExpr: Parser[LiteralStringExpr] =
     "'" ~ """[a-z*]+""".r ~ "'" ^^ { case q1 ~ s ~ q2 => LiteralStringExpr(s) }

  def where: Parser[Expression] = "where" ~ expression ^^ {case whereEx ~ expr => expr}
  def selectFrom: Parser[SqlQuery] = "select" ~ fields ~ "from" ~ word ~ opt(where) ^^ {
    case select ~ fs ~ from ~ wd ~ where => SqlQuery(fs, wd, where)}

  def word: Parser[String] =
    """[a-z]+""".r       ^^ { _.toString }

  def parse(sql: String): Try[SqlQuery] = parse(phrase(selectFrom), sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg ))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }
}
