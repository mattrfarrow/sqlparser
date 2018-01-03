package com.mrfarrow.sqlparser

import com.mrfarrow.sqlparser.expressions._

import scala.util.Try
import scala.util.parsing.combinator._


object SqlParser {

  def parse(sql: String, thingToStrings: ThingToStrings[_]): Try[SqlQuery] = {
    new SqlParser(thingToStrings).parse(sql)
  }

}

class SqlParser(thingToStrings: ThingToStrings[_]) extends RegexParsers {
  def parse(sql: String): Try[SqlQuery] = parse(phrase(selectFrom), sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg ))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }

  private def expression: Parser[_ <: Expression] =
    bracketedExpression | andExpression | notExpression | comparitiveExpr | lengthExpr | field | literalExpression
  private def standaloneExpression: Parser[_ <: Expression] =
    bracketedExpression| notExpression  | lengthExpr | field | literalExpression | comparitiveExpr
  private def bracketedExpression: Parser[Expression] =
    ("(" ~ expression ~ ")") ^^ {case b ~ ex ~ b2 => ex}

  private def notExpression: Parser[NotExpression] =
    "not " ~ expression ^^ { case not ~ expr => NotExpression(expr) }

  private def andExpression: Parser[AndExpression] =
    standaloneExpression ~ "and" ~ standaloneExpression ^^ { case e1 ~ and ~ e2 => AndExpression(e1,e2)}

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
