package com.mrfarrow.sqls

import java.io.File

import com.mrfarrow.sqlparser._

import scala.util.{Failure, Success}


object Sqls {

  def main(args: Array[String]): Unit = {

    val sql = args.headOption.getOrElse("One parameter expected - the SQL")

    val currentDir = System.getProperty("user.dir")
    println("Running from "+ currentDir)

    println(runLs(new File(currentDir), sql))
  }

  def runLs(workingDir: File, sql: String) = {
    val files = workingDir.listFiles()

    val thingsToStrings = new FileToStrings

    println("Parsing " + sql)
    val query = new SqlParser(thingsToStrings).parse(sql) match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    ParserUtil.process(files.toList, thingsToStrings, query)
  }

}