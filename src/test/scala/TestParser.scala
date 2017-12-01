import org.junit.Test
import org.junit.Assert._

import scala.util.{Failure, Success}


class TestParser {

  @Test
  def one_selection() = {
      genericTest(
        "select species from whatever",
        """cat
          |hamster""".stripMargin)
  }

  @Test
  def two_selections() = {
    genericTest(
      "select species,name from whatever",
      """cat|fluff
        |hamster|Terry""".stripMargin)
  }

  @Test
  def with_a_filter() = {
    genericTest(
      "select species from whatever where species='cat'",
      """cat""".stripMargin)
  }

  @Test
  def with_wildcard_at_end() = {
    genericTest(
      "select species, name from whatever where species like 'c*'",
      """cat|fluff""".stripMargin)
  }

  @Test
  def with_wildcard_at_start() = {
    genericTest(
      "select species, name from whatever where species like '*t'",
      """cat|fluff""".stripMargin)
  }

  @Test
  def with_wildcard_at_both_ends() = {
    genericTest(
      "select species, name from whatever where name like '*luf*'",
      """cat|fluff""".stripMargin)
  }

  @Test
  def with_wildcard_no_match() = {
    genericTest(
      "select species, name from whatever where species like 'cu*'",
      """""".stripMargin)
  }

  private def genericTest(sql: String, expected: String): Unit = {
    val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

    val animalToString = new AnimalToString

    val query = new TestSqlParser(animalToString).parse(sql) match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    assertEquals(expected, ParserUtil.process(animals, animalToString, query))
  }

  case class Animal(name: String, species: String)

  class AnimalToString extends ThingToStrings[Animal] {
    override def getString(name: String, obj: Animal): String = name match {
      case "name" => obj.name
      case "species" => obj.species
      case x => throw new Exception("No field " + x)
    }

    override def getBoolean(name: String, obj: Animal): Boolean = throw new UnsupportedOperationException

    override def getType(name: String): ExpressionType = ExpressionType.String
  }
}

