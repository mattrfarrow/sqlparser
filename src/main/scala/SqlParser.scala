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

case class Comma()
case class From()

case class EqualsExpr(left: Expression, right: Expression, thingToStrings: ThingToStrings[_]) extends Expression {
  if(left.getType(thingToStrings) != right.getType(thingToStrings)) {
    throw new Exception("Left side type is " + left.getType(thingToStrings) + " right side is " + right.getType(thingToStrings))
  }

  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = {
    left.getType(thingToStrings) match {
      case ExpressionType.Boolean => left.evaluateBool(thingToStrings, obj) == right.evaluateBool(thingToStrings, obj)
      case ExpressionType.String  => left.evaluateString(thingToStrings, obj) == right.evaluateString(thingToStrings, obj)
      case _       => throw new IllegalStateException()
    }
  }

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = throw new UnsupportedOperationException

  override def getType[T](thingToStrings:  ThingToStrings[T]): ExpressionType = left.getType(thingToStrings)
}

case class LiteralStringExpr(string: String) extends Expression {
  override def evaluateBool[T](thingToStrings: ThingToStrings[T], obj: T): Boolean = throw new UnsupportedOperationException

  override def evaluateString[T](thingToStrings: ThingToStrings[T], obj: T): String = string

  override def getType[T](thingToStrings: ThingToStrings[T]): ExpressionType = ExpressionType.String
}
case class Where(expr: Expression)


class SqlParser(thingToStrings: ThingToStrings[_]) extends RegexParsers {

  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def expression: Parser[_ <: Expression] =   equalsExpression | field | literalStringExpr

  def bracketedExpression: Parser[Expression] = ("(" ~ expression ~ ")") ^^ {case b ~ ex ~ b2 => ex}
  def standaloneExpression: Parser[_ <: Expression] = literalStringExpr | field | bracketedExpression

  def equalsExpression: Parser[EqualsExpr] ={
    standaloneExpression ~ "=" ~ standaloneExpression ^^ { case left ~ eq ~ right  => EqualsExpr(left, right, thingToStrings) }
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

class TestSqlParser(thingToStrings: ThingToStrings[_]) extends SqlParser(thingToStrings) {

  def parse(sql: String): Try[SqlQuery] = parse(phrase(selectFrom), sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,remaining) => scala.util.Failure(new Exception("Parser failed: "+msg ))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }
}