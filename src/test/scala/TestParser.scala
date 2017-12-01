import org.junit.Test
import org.junit.Assert._

import scala.util.{Failure, Success}


class TestParser {

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

  @Test
  def one_selection(): Unit = {
      val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

      val animalToString = new AnimalToString

      val query = new TestSqlParser(animalToString).parse("select species from whatever") match {
        case t: Success[SqlQuery] => t.value
        case t: Failure[_] => throw t.exception
      }
      assertEquals(
        """cat
          |hamster""".stripMargin,
        ParserUtil.process(animals, animalToString, query))
  }

  @Test
  def two_selections(): Unit = {
    val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

    val animalToString = new AnimalToString

    val query = new TestSqlParser(animalToString).parse("select species,species from whatever") match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }
    assertEquals(
      """cat|cat
        |hamster|hamster""".stripMargin,
      ParserUtil.process(animals, animalToString, query))
  }

  @Test
  def with_a_filter(): Unit = {
    val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

    val animalToString = new AnimalToString

    val query = new TestSqlParser(animalToString).parse("select species from whatever where species='cat'") match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    println("Query: " + query)

    assertEquals(
      """cat""".stripMargin,
      ParserUtil.process(animals, animalToString, query))
  }
}

