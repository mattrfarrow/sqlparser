package com.mrfarrow.sqlparser

import scala.util.{Failure, Success}

/**
  * Created by matt.farrow on 30/11/2017.
  */
object FileSystemTest {

  case class Animal(name: String, species: String)

  def main(args: Array[String]): Unit = {
    val animals = List(Animal("fluff", "cat"), Animal("Terry","hamster"))

    val animalToString = new AnimalToString

    val query = new TestSqlParser(animalToString).parse("select species, species from whatever") match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }
    println(ParserUtil.process(animals, animalToString, query))

  }

  class AnimalToString extends ThingToStrings[Animal] {
    override def getString(name: String, obj: Animal): String = name match {
      case "name" => obj.name
      case "species" => obj.species
      case x => throw new Exception("No field " + x)
    }

    override def getBoolean(name: String, obj: Animal): Boolean = throw new UnsupportedOperationException("No boolean fields here")

    override def getType(name: String): ExpressionType = ExpressionType.String
  }
}
