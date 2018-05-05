package realnumberencoding.tokenizing

object UppercaseLettersPlusSpaceTokenizing extends SelectedCharsStringTokenizing {
  override protected val chars = Set(' ') ++ ('A' to 'Z')
  override val description = "Uppercase letters plus space"
}
