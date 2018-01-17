package com.mrfarrow.sqlparser

import com.mrfarrow.sqlparser.expressions._

import scala.util.Try
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader


object SqlParser {
  def parse(sql: String, thingToStrings: ThingToStrings[_]): Try[SqlQuery] = {
    new SqlParser(thingToStrings).parse(sql)
  }
}

class SqlParser(thingToStrings: ThingToStrings[_]) extends RegexParsers with PackratParsers {
  def parse(sql: String): Try[SqlQuery] = parse(phrase(selectFrom), new PackratReader[Char](new CharSequenceReader(sql))) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg ))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }

  private lazy val expression: PackratParser[_ <: Expression] =
    bracketedExpr | andExpr | notExpr | comparitiveExpr | lengthExpr | field | literalExpr

  private lazy val bracketedExpr: PackratParser[Expression] =
    ("(" ~ expression ~ ")") ^^ {case b ~ ex ~ b2 => ex}

  private lazy val notExpr: PackratParser[NotExpression] =
    "not " ~ expression ^^ { case not ~ expr => NotExpression(expr) }

  private lazy val andExpr: PackratParser[AndExpression] =
    expression ~ "and" ~ expression ^^ { case e1 ~ and ~ e2 => AndExpression(e1,e2)}

  private lazy val equalsExpr: PackratParser[EqualsExpr] =
    expression ~ "=" ~ expression ^^ { case left ~ eq ~ right  => EqualsExpr(left, right) }

  private lazy val greaterThanExpr: PackratParser[GreaterThanExpression] =
    expression~ ">" ~ expression ^^ { case left ~ gt ~ right  => GreaterThanExpression(left, right) }

  private lazy val lessThanExpr: PackratParser[LessThanExpression] =
    expression~ "<" ~ expression ^^ { case left ~ gt ~ right  => LessThanExpression(left, right) }

  private lazy val likeExpr: PackratParser[LikeExpr] ={
    expression ~ "like" ~ literalStringExpr ^^ { case left ~ like ~ right  => LikeExpr(left, right) }
  }

  private lazy val comparitiveExpr = equalsExpr | likeExpr | greaterThanExpr | lessThanExpr

  private lazy val lengthExpr: PackratParser[LengthExpr] ={
    "length" ~ bracketedExpr ^^ { case length ~ e  => new LengthExpr(e) }
  }

  private lazy val field: PackratParser[Expression]   = """[a-z]+""".r       ^^ { s => FieldExpr(s) }
  private lazy val fields: PackratParser[Array[Expression]]  = expression ~ opt("," ~ fields) ^^ {
    case f ~ Some(comma ~ fs) => Array(f) ++ fs
    case f ~ None => Array(f)}

  private lazy val literalStringExpr: PackratParser[LiteralStringExpr] =
     "'" ~ """[a-z*]+""".r ~ "'" ^^ { case q1 ~ s ~ q2 => LiteralStringExpr(s) }

  private lazy val literalIntExpr: PackratParser[LiteralIntExpr] =
    """(0|[1-9]\d*)""".r ^^ {s => LiteralIntExpr(s.toInt) }

  private lazy val literalExpr: PackratParser[Expression] = literalIntExpr | literalStringExpr

  private lazy val directoryPath: PackratParser[String] = """[/.A-Za-z]+""".r ^^ { x => x }

  private lazy val from: PackratParser[String] = "from" ~ directoryPath ^^ { case from ~ place => place }
  private lazy val where: PackratParser[Expression] = "where" ~ expression ^^ {case whereEx ~ expr => expr}

  private lazy val selectFrom: PackratParser[SqlQuery] = "select" ~ fields ~ opt(from) ~ opt(where) ^^ {
    case select ~ fs ~ from  ~ where => SqlQuery(fs, from, where)
  }
}
