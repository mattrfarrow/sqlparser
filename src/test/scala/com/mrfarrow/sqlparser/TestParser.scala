package com.mrfarrow.sqlparser

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

class TestParser extends FlatSpec with Matchers {

  it should "work with one selection" in {
      genericTest(
        "select species from whatever",
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

  it should "work with a wildcard at the start" in {    genericTest(
      "select species, name from whatever where species like '*t'",
      """cat|fluff""".stripMargin)
  }
  it should "work with a wildcard at both ends" in {
    genericTest(
      "select species, name from whatever where name like '*luf*'",
      """cat|fluff""".stripMargin)
  }

  it should "returns no results ofr a non matching wildcard" in {
    genericTest(
      "select species, name from whatever where species like 'cu*'",
      """""".stripMargin)
  }

  private def genericTest(sql: String, expected: String): Unit = {
    val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

    val animalToString = new AnimalToString

    val query = SqlParser.parse(sql, animalToString) match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    ParserUtil.process(animals, animalToString, query) should be(expected)
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

