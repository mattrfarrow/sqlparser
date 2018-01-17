package com.mrfarrow.sqlparser

import scala.util.Try
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader


class PRExpression
case class FalseExpr() extends PRExpression
case class TrueExpr() extends PRExpression
case class AndExpr(expr1: PRExpression, expr2: PRExpression) extends PRExpression

object PackratSqlParser {
  def parse(sql: String): Try[PRExpression] = new PackratSqlParser().parse(sql)
}

class PackratSqlParser extends RegexParsers with PackratParsers {
  def parse(sql: String): Try[_ <: PRExpression] = parseAll(expression, new PackratReader[Char](new CharSequenceReader(sql))) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg + "remaining: "+ remaining.source.toString.drop(remaining.offset)))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }

  private lazy val expression: PackratParser[_ <: PRExpression] =
    andExpr | falseExpr | trueExpr

  private lazy val falseExpr: PackratParser[FalseExpr] =
    "false" ^^ (_ => FalseExpr())

  private lazy val trueExpr: PackratParser[TrueExpr] =
    "true" ^^ (_ => TrueExpr())

  private lazy val andExpr: PackratParser[PRExpression] =
    expression ~ "and" ~ expression ^^ { case e1 ~ and ~ e2 => AndExpr(e1,e2)}

}
