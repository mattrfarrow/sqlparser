package com.mrfarrow.sqlparser


object ParserRegex {

  def createRegexFromGlob(glob: String): String = {
    val out = new StringBuilder("^")
    var i = 0
    while ( {
      i < glob.length
    }) {
      val c = glob.charAt(i)
      c match {
        case '*' =>
          out.append(".*")
        case '?' =>
          out.append('.')
        case '.' =>
          out.append("\\.")
        case '\\' =>
          out.append("\\\\")
        case _ =>
          out.append(c)
      }

      {
        i += 1; i
      }
    }
    out.append('$')
    out.toString
  }

}
