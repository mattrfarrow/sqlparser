/**
  * Created by matt.farrow on 30/11/2017.
  */
object ParserUtil {

  def process[T](objects: List[T], wordToString: ThingToStrings[T], sqlQuery: SqlQuery): String  = {
    val filtered = sqlQuery.where match {
      case Some(expression) =>  println("Evaluating where clause: " + expression)
        objects.filter(obj => expression.evaluate(wordToString, obj))
      case None => objects
    }

    val resp: List[String] =
      filtered.map(o => {
      sqlQuery.fields.map(field => wordToString.getString(field.name, o)).mkString("|")
    })

    resp.mkString("\n")
  }

}
