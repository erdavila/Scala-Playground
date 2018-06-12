package realnumberencoding.tokenizing

trait SelectedCharsStringTokenizing extends StringTokenizing {
  protected val chars: Set[Char]

  private lazy val charAt = chars.toIndexedSeq.sorted
  private lazy val tokenFor = charAt.zipWithIndex.toMap

  override lazy val tokenValues = chars.size

  def tokenize(string: String): Stream[Token] =
    string.toStream.map { char =>
      tokenFor.get(char) match {
        case Some(token) => token
        case None => throw new StringTokenizing.Exception
      }
    }

  def detokenize(tokens: Stream[Token]): String =
    tokens.map(charAt).mkString
}
