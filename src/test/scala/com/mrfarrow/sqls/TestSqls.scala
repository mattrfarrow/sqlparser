package com.mrfarrow.sqls

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class TestSqls extends FlatSpec with Matchers {

  it should "list files" in {
    val sql = """select name where name like 'short*'"""
    Sqls.runLs(new File("src/test/resources"), sql) should be ("shortFile.txt")
  }

}
