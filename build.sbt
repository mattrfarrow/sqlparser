name := "sqlparser"

version := "1.0"

scalaVersion := "2.11.12"

libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a"))