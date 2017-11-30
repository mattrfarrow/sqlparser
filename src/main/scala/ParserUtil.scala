/**
  * Created by matt.farrow on 30/11/2017.
  */
object ParserUtil {

  def process[T](objects: List[T], wordToString: ThingToStrings[T], sqlQuery: SqlQuery): String  = {
    val resp: List[String] =
      objects.map(o => {
      sqlQuery.fields.map(field => wordToString.getString(field.name, o)).mkString("|")
      })

    resp.mkString("\n")
  }

}
