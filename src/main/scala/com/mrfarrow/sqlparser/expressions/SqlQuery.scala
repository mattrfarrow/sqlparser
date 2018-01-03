package com.mrfarrow.sqlparser.expressions

case class SqlQuery(fields: Array[Expression], from: Option[String], where: Option[Expression]) {
  override def toString: String = "SELECT " + fields.mkString(",") + " FROM " + from + where.map(e => " WHERE "+e)
}
