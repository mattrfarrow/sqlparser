import scala.util.Try
import scala.util.parsing.combinator._


case class Select()
case class SqlQuery(fields: Array[FieldExpr], from: String) {
  override def toString(): String = "SELECT " + fields.mkString(",") + " FROM " + from

  def apply(field: FieldExpr, from: String) = SqlQuery(Array(field), from)
}

class Expression()
case class FieldExpr(name: String) extends Expression
case class EqualsSign()
case class Comma()
case class From()

case class EqualsExpr(left: Expression, right: Expression) extends Expression
case class LiteralStringExpr(string: String) extends Expression


class SqlParser extends RegexParsers {
  def equalsSign: Parser[EqualsSign] = """=""".r ^^ {_ => EqualsSign()}
  def comma: Parser[Comma]   = ",".r   ^^ { _ => Comma() }
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def field: Parser[FieldExpr]   = """[a-z]+""".r       ^^ { s => FieldExpr(s) }
  def fields: Parser[Array[FieldExpr]]  = field ~ opt(comma ~ fields) ^^ {
    case f ~ Some(comma ~ fs) => Array(f) ++ fs
    case f ~ None => Array(f)}

  def expression: Parser[_ <: Expression] = field | equalsExpression | literalStringExpr

  def equalsExpression: Parser[EqualsExpr] =
    expression ~ equalsSign ~ expression ^^ { case left ~ eq ~ right  => EqualsExpr(left, right) }

  def literalStringExpr: Parser[LiteralStringExpr] =
    "[a-z]+".r ^^ { s => LiteralStringExpr(s.drop(1).dropRight(1)) }

  def select: Parser[Select] = "select".r ^^ {_ => Select()}
  def from: Parser[From] = "from".r ^^ {_ => From()}
  def selectFrom: Parser[SqlQuery] = (select ~ fields ~ from ~ word) ^^ {case select ~ fs ~ from ~ wd => SqlQuery(fs, wd)}
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }

}

object TestSqlParser extends SqlParser {

  def parse(sql: String): Try[SqlQuery] = parse(selectFrom, sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,_) => scala.util.Failure(new Exception("Parser failed: "+msg))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }
}