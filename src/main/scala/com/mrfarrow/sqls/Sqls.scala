package com.mrfarrow.sqls

import java.io.File

import com.mrfarrow.sqlparser._

import scala.util.{Failure, Success}


object Sqls {

  def main(args: Array[String]): Unit = {

    val sql = args.headOption.getOrElse("One parameter expected - the SQL")

    val currentDir = System.getProperty("user.dir")
    println("Running from "+ currentDir)

    runLs(new File(currentDir), sql)
  }

  def runLs(workingDir: File, sql: String) = {
    val files = workingDir.listFiles()


    val thingsToStrings = new ThingToStrings[File] {
      override def getType(name: String): ExpressionType = name match {
        case "name" => ExpressionType.String
        case "size" => ExpressionType.Integer
        case "type" => ExpressionType.String
        case "isdir" => ExpressionType.Boolean
      }

      override def getString(name: String, obj: File): String = name match {
        case "name" => obj.getName
        case "type" => if (obj.isDirectory) "dir" else "file"
      }

      override def getBoolean(name: String, obj: File): Boolean = name match {
        case "isdir" => obj.isDirectory
      }

      override def getInt(name: String, obj: File): Int = name match {
        case "size" => obj.length().asInstanceOf[Int]
      }
    }

    println("Parsing " + sql)
    val query = new TestSqlParser(thingsToStrings).parse(sql) match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    System.out.println(ParserUtil.process(files.toList, thingsToStrings, query))
  }

}
