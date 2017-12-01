import org.junit.Test
import org.junit.Assert._

import scala.util.{Failure, Success}


class TestParser {

  @Test
  def one_selection(): Unit = {
      genericTest("select species from whatever",
        """cat
          |hamster""".stripMargin)
  }

  @Test
  def two_selections(): Unit = {
    genericTest("select species,species from whatever",
      """cat|cat
        |hamster|hamster""".stripMargin)
  }

  @Test
  def with_a_filter(): Unit = {
    genericTest("select species from whatever where species='cat'", """cat""".stripMargin)
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

