import scala.annotation.tailrec

object ImportsReformatter extends App {
  val Import = "import "
  val Arrow = "=>"

  sealed trait Name { val name: String; def decl: String }
  case class UnaliasedName(name: String) extends Name { def decl = name }
  case class AliasedName(name: String, alias: String) extends Name { def decl = s"$name $Arrow $alias" }

  loop()

  @tailrec
  def loop(): Unit = {
    println("Entre linhas import de Código Scala, finalizando com uma linha em branco.")
    println("Use CTRL+D para sair.")
    println()
    val lines = getLines()
    if (lines.nonEmpty) {
      reformat(lines)
      loop()
    }
  }

  @tailrec
  def getLines(lines: Seq[String] = Seq.empty): Seq[String] =
    Option(Console.in.readLine()) match {
      case Some(line) if line.isEmpty && lines.isEmpty => getLines()
      case Some(line) if line.isEmpty => lines
      case Some(line) => getLines(lines :+ line)
      case None => lines
    }

  def reformat(lines: Seq[String]): Unit = {
    val decls = lines
      .map(_.trim())
      .filter(_.nonEmpty)
      .map(parse)
      .groupBy { case (packge, names) => packge }
      .mapValues { items =>
        items.flatMap { case (packge, names) => names }.toSeq.sortBy(_.name).distinct
      }

    val formattedLines = decls.map { case (packge, names) =>
      val formattedNames = names match {
        case Seq(name: UnaliasedName) => name.name
        case Seq(name: AliasedName) => namesList(names)
        case _ => namesList(names)
      }

      Import + packge + '.' + formattedNames
    }

    for (formattedLine <- formattedLines.toSeq.sorted) {
      println(formattedLine)
    }
    println()
  }

  def parse(line: String): (String, Seq[Name]) = {
    assert(line startsWith Import)
    val content = line.drop(Import.length()).trim()

    val separatorPos = content.lastIndexOf('.')
    assert(separatorPos >= 0)
    val (packge, dotAndRest) = content.splitAt(separatorPos)

    assert(dotAndRest(0) == '.')
    val rest = dotAndRest.drop(1).trim()

    val names = if (rest.head == '{') {
      assert(rest.last == '}')
      val parts = rest.drop(1).dropRight(1)
        .split(',')
        .map(_.trim())
        .map { part =>
          val arrowPos = part.indexOf(Arrow)
          if (arrowPos >= 0) {
            val name = part.take(arrowPos)
            val alias = part.drop(arrowPos + Arrow.length())
            AliasedName(name.trim(), alias.trim())
          } else {
            UnaliasedName(part)
          }
        }
      parts.toSeq
    } else {
      Seq(UnaliasedName(rest))
    }

    (packge.trim(), names)
  }

  def namesList(namesAndAliases: Seq[Name]): String =
    namesAndAliases.map(_.decl).mkString("{", ", ", "}")
}
