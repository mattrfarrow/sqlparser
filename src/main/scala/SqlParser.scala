import scala.util.Try
import scala.util.parsing.combinator._


case class Select()
case class SqlQuery(fields: Array[Field], from: String) {
  override def toString(): String = "SELECT " + fields.mkString(",") + " FROM " + from
}
case class Field(name: String)
case class Comma()
case class From()


class SqlParser extends RegexParsers {
  def comma: Parser[Comma]   = ",".r   ^^ { _ => Comma() }
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def fieldorfields = fields | field
  def field: Parser[Array[Field]]   = """[a-z]+""".r       ^^ { s => Array(Field(s)) }
  def fields: Parser[Array[Field]]  = field ~ comma ~ field ^^ {case f ~ comma ~ fs => f ++ fs}
  def select: Parser[Select] = "select".r ^^ {_ => Select()}
  def from: Parser[From] = "from".r ^^ {_ => From()}
  def selectFrom: Parser[SqlQuery] = (select ~ fieldorfields ~ from ~ word) ^^ {case select ~ fs ~ from ~ wd => SqlQuery(fs, wd)}
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }

}

object TestSqlParser extends SqlParser {
  def main(args: Array[String]): Unit = {
    parse(selectFrom, "select arse from cheese") match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }

    parse(selectFrom, "select arse, buckets from cheese") match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
  }

  def parse(sql: String): Try[SqlQuery] = parse(selectFrom, sql) match {
    case Success(matched,_) => scala.util.Success(matched)
    case Failure(msg,_) => scala.util.Failure(new Exception("Parser failed"))
    case Error(msg,_) => scala.util.Failure(new Exception(msg))
  }
}