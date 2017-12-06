package com.mrfarrow.sqls

import java.io.File

import com.mrfarrow.sqlparser.{ParserUtil, SqlParser, SqlQuery}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}


class TestSqls extends FlatSpec with Matchers {

  it should "list files " in {
    val resourcesDirectory = new File("src/test/resources")

    val sql = """select name where name like 'sr*' """
    val query = new SqlParser(new FileToStrings).parse(sql) match {
      case t: Success[SqlQuery] => t.value
      case t: Failure[_] => throw t.exception
    }

    val currentDir = new File(System.getProperty("user.dir"))
    Sqls.runLs(currentDir, sql) should be ("src")
  }

}
