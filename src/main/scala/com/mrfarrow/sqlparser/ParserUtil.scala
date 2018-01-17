package com.mrfarrow.sqlparser

import com.mrfarrow.sqlparser.expressions.{Expression, SqlQuery}

object ParserUtil {

  def getMaximumFieldLengths(resp: List[Array[String]]): Array[Int] = {
    assert(resp.nonEmpty)
    val maxSizes = new Array[Int](resp.head.length)
    resp.foreach { record =>

      var fieldNumber = 0
      record.indices.foreach( fieldNumber =>
        maxSizes(fieldNumber) = Math.max(maxSizes(fieldNumber), record(fieldNumber).length))
    }
    maxSizes
  }


  def process[T](objects: List[T], wordToString: ThingToStrings[T], sqlQuery: SqlQuery, pipesNotSpaces: Boolean): String  = {
    val filtered = sqlQuery.where match {
      case Some(expression) =>  objects.filter(obj => expression.evaluateBool(wordToString, obj))
      case None => objects
    }

    def sorter(expressions: Array[Expression])(a: T, b: T): Boolean = {
      for(expression <- expressions) {
        val comparison = expression.getType(wordToString) match {
          case ExpressionType.Boolean => expression.evaluateBool(wordToString, a).compareTo(expression.evaluateBool(wordToString, b))
          case ExpressionType.Integer => expression.evaluateInt(wordToString, a).compareTo(expression.evaluateInt(wordToString, b))
          case ExpressionType.String => expression.evaluateString(wordToString, a).compareTo(expression.evaluateString(wordToString, b))
          case t => throw new RuntimeException("Don't know what to do for type " + t)
        }

        if (comparison < 0) {
          return true
        } else if (comparison > 0) {
          return false
        }
      }

      true
    }

    val ordered = sqlQuery.orderBy match {
      case Some(o) => filtered.sortWith(sorter(o))
      case None => filtered
    }

    val resp: List[Array[String]] =
      ordered.map(o => {
        sqlQuery.fields.map(field => field.evaluateString(wordToString, o))
      })

    if(pipesNotSpaces) {
      resp.map(_.mkString("|")).mkString("\n")
    } else if(resp.isEmpty) {
      ""
    } else {
      val fieldLengths: Array[Int] = getMaximumFieldLengths(resp)
      resp.map(record =>
        record.indices.map(index =>
          if(index == record.indices.last) {
            record(index)
          } else {
            record(index).padTo(fieldLengths(index) + 1, ' ')
          }
        ).mkString("")
      ).mkString("\n")
    }
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
