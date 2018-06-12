package realnumberencoding

object StringDisplay {

  private val LightRed = "31;1"
  private val LightGreen = "32;1"
  private val LightYellow = "33;1"

  private val DelimiterQuotes = ansiColor(LightRed)("\"")

  def display(string: String): String =
    string.map {
      case '\\' => ansiColor(LightGreen)("\\")
      case char if char < 32 => ansiColor(LightYellow)("\\%x".format(char & 0x00FFFF))
      case char => char.toString
    }.mkString(DelimiterQuotes, "", DelimiterQuotes)

  private def ansiColor(code: String)(string: String): String =
    ansiCode(code) + string + ansiCode("0")

  private def ansiCode(code: String): String =
    "\u001b[" + code + "m"
}
