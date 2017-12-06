package com.mrfarrow.sqlparser

/**
  * Created by matt.farrow on 30/11/2017.
  */
object ParserUtil {

  def process[T](objects: List[T], wordToString: ThingToStrings[T], sqlQuery: SqlQuery): String  = {
    val filtered = sqlQuery.where match {
      case Some(expression) =>  objects.filter(obj => expression.evaluateBool(wordToString, obj))
      case None => objects
    }

    val resp: List[String] =
      filtered.map(o => {
      sqlQuery.fields.map(field => getStringRepr(field.name, o, wordToString)).mkString("|")
    })

    resp.mkString("\n")
  }

  def getStringRepr[T](fieldName: String, obj: T, thingToStrings: ThingToStrings[T]): String = {

    val str = thingToStrings.getType(fieldName) match {
      case ExpressionType.Integer => thingToStrings.getInt(fieldName, obj)
      case ExpressionType.String => thingToStrings.getString(fieldName, obj)
      case ExpressionType.Boolean => thingToStrings.getBoolean(fieldName, obj)
    }

    str.toString
  }

}
