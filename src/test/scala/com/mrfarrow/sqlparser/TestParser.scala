package com.mrfarrow.sqlparser

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class TestParser extends FlatSpec with Matchers {

  it should "work with one selection" in {
      genericTest(
        "select species",
        """cat
          |hamster""".stripMargin)
  }

  it should "work with two selections" in {
    genericTest(
      "select species,name from whatever",
      """cat|fluff
        |hamster|Terry""".stripMargin)
  }

  it should "work with a filter" in {
    genericTest(
      "select species from whatever where species='cat'",
      """cat""".stripMargin)
  }

  it should "work with a wildcard at the end" in {
    genericTest(
      "select species, name from whatever where species like 'c*'",
      """cat|fluff""".stripMargin)
  }

  it should "work with a wildcard at the start" in {
    genericTest(
      "select species, name from whatever where species like '*t'",
      """cat|fluff""".stripMargin)
  }
  it should "work with a wildcard at both ends" in {
    genericTest(
      "select species, name from whatever where name like '*luf*'",
      """cat|fluff""".stripMargin)
  }

  it should "returns no results for a non matching wildcard" in {
    genericTest(
      "select species, name from whatever where species like 'cu*'",
      """""".stripMargin)
  }

  it should "support length()" in {
    genericTest(
      "select species, length(species) from whatever",
      """cat|3
        |hamster|7""".stripMargin)
  }

  it should "support greater-than" in {
    genericTest(
      "select species where length(species)>5",
      """hamster""".stripMargin)
  }

  it should "support less-than" in {
    genericTest(
      "select species where length(species)<5",
      """cat""".stripMargin)
  }

  it should "support not" in {
    genericTest(
      "select species where not length(species)>5",
      """cat""".stripMargin)
  }

  it should "support and" in {
    genericTest(
      "select species where not length(species)>5 and not length(species)>4",
      """cat""".stripMargin)
  }

  it should "support and 2" in {
    genericTest(
      "select species where not length(species)>1 and not length(species)>5",
      """""".stripMargin)
  }

  it should "support and 3" in {
    genericTest(
      "select species where not length(species)>5 and not length(species)>1",
      """""".stripMargin)
  }


  private def genericTest(sql: String, expected: String): Unit = {
    val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

    val animalToString = new AnimalToString

    val query = SqlParser.parse(sql, animalToString) match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    ParserUtil.process(animals, animalToString, query, pipesNotSpaces = true) should be(expected)
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

    override def getInt(name: String, obj: Animal): Int = 0
  }
}

