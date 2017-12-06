package com.mrfarrow.sqls

import java.io.File

import com.mrfarrow.sqlparser.{ExpressionType, ThingToStrings}

class FileToStrings extends ThingToStrings[File] {
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