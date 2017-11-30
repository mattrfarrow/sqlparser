import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator._


case class SqlQuery(fields: Array[FieldExpr], from: String, where: Option[Expression]) {
  override def toString: String = "SELECT " + fields.mkString(",") + " FROM " + from + where.map(e => " WHERE "+e)
}

abstract class Expression() {
  def evaluate[T](thingToStrings: ThingToStrings[T], obj: T): Boolean
  def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String
}

case class FieldExpr(name: String) extends Expression {
  override def evaluate[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = thingToStrings.getBoolean(name, obj)
  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = thingToStrings.getString(name, obj)
}

case class Comma()
case class From()

case class EqualsExpr(left: Expression, right: Expression) extends Expression {

  override def evaluate[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = left.evaluate(thingToStrings, obj) == right.evaluate(thingToStrings, obj)

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = throw new UnsupportedOperationException
}

case class LiteralStringExpr(string: String) extends Expression {
  override def evaluate[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = throw new UnsupportedOperationException

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = string
}
case class Where(expr: Expression)


class SqlParser extends RegexParsers {

  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def expression: Parser[_ <: Expression] =   field | equalsExpression  |  literalStringExpr

  def equalsExpression: Parser[EqualsExpr] ={
    expression ~ "=" ~ expression ^^ { case left ~ eq ~ right  => EqualsExpr(left, right) }
  }

  def field: Parser[FieldExpr]   = """[a-z]+""".r       ^^ { s => FieldExpr(s) }
  def fields: Parser[Array[FieldExpr]]  = field ~ opt("," ~ fields) ^^ {
    case f ~ Some(comma ~ fs) => Array(f) ++ fs
    case f ~ None => Array(f)}

  def literalStringExpr: Parser[LiteralStringExpr] =
     "'" ~ """[a-z]+""".r ~ "'" ^^ { case q1 ~ s ~ q2 => LiteralStringExpr(s) }

  def where: Parser[Expression] = "where" ~ expression ^^ {case whereEx ~ expr => expr}
  def selectFrom: Parser[SqlQuery] = "select" ~ fields ~ "from" ~ word ~ opt(where) ^^ {
    case select ~ fs ~ from ~ wd ~ where => SqlQuery(fs, wd, where)}

  def word: Parser[String] =
    """[a-z]+""".r       ^^ { _.toString }

}

object TestSqlParser extends SqlParser {

  def parse(sql: String): Try[SqlQuery] = parse(phrase(selectFrom), sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg ))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }
}