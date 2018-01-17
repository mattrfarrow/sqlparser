package com.mrfarrow.sqls

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class TestSqls extends FlatSpec with Matchers {

  it should "list files" in {
    val sql = """select name order by name"""
    Sqls.runLs(new File("src/test/resources"), sql) should be (
      "colours\n" +
      "longerFile.txt\n" +
      "shortFile.txt"
    )
  }

  it should "list files with a WHERE" in {
    val sql = """select name where name like 'short*'"""
    Sqls.runLs(new File("src/test/resources"), sql) should be ("shortFile.txt")
  }

  it should "pad fields" in {
    val sql = """select name, type where type like 'fil*' order by name"""
    Sqls.runLs(new File("src/test/resources"), sql) should be (
      "longerFile.txt file\n" +
      "shortFile.txt  file"
      )
  }

  it should "select from dir specified in WHERE" in {
    val sql = """select name from src/test/resources/colours/"""
    Sqls.runLs(new File("src/test/resources"), sql) should be (
      "red.txt"
    )
  }

  it should "handle a WHERE clause that excludes everything" in {
    val sql = """select name from src/test/resources/colours/ where length(name) > 9999"""
    Sqls.runLs(new File("src/test/resources"), sql) should be (
      ""
    )
  }

  it should "error on selecting from a non-existing dir" in {
    val sql = """select name, type from /wahhh"""
    assertThrows[Exception](
      Sqls.runLs(new File("src/test/resources"), sql)
    )
  }

}
