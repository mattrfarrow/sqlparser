import scala.util.parsing.combinator._


case class SelectFrom()
case class Select(from: String)

class SqlParser extends RegexParsers {
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def select: Parser[SelectFrom]   = "select from".r       ^^ (_ => SelectFrom())

  def statement: Parser[Select] = select ~ word ^^ { case select ~ wd => Select(wd)}
}

object TestSqlParser extends SqlParser {
  def main(args: Array[String]): Unit = {
    parse(statement, "select from cheese") match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
  }
}