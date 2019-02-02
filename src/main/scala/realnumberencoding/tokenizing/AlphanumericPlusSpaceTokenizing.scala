package realnumberencoding.tokenizing

object AlphanumericPlusSpaceTokenizing extends SelectedCharsStringTokenizing {
  override protected val chars = Set(' ') ++ ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')
  override val description = "Alphanumeric chars plus space"
}
