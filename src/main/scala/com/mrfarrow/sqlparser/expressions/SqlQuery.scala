package com.mrfarrow.sqlparser.expressions

case class SqlQuery(fields: Array[Expression], from: Option[String], where: Option[Expression], orderBy: Option[Array[Expression]]) {
  override def toString: String = "SELECT " + fields.mkString(",") + " FROM " + from + where.map(e => " WHERE " +e) + orderBy.map(ob => " ORDER BY " +ob)
}
