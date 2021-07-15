package crackingthecodinginterview.arraysandstrings

object StringCompression {

  // O(n) (runs through the string once and uses StringBuilder which itself uses O(n) time)
  // We could reduce uses of the StringBuilder wher no compression is possible
  // (which might make some constant-factor time improvements, and certainly could reduce space), by either:
  //  - keeping running total of the net improvements in length -
  //    this could avoid having to build the string at the end
  //  - doing a first pass through (O(n)) to check if there would be a net improvement -
  //    this would be throw-away where compression is possible,
  //    but would remove the need to allocate or append to the StringBuilder where it is not)
  def compressString(s: String): String = {
    val builder = new StringBuilder()
    var activeChar: Option[Char] = None
    var currentSeriesLength: Int = 0

    val flush = { () => activeChar.map { charValue => builder.append(s"$charValue$currentSeriesLength") } }

    s.foreach { char =>
      if(activeChar.contains(char)) {
        currentSeriesLength += 1
      }
      else {
        flush()
        activeChar = Some(char)
        currentSeriesLength = 1
      }
    }
    flush()
    val newString = builder.toString()
    if(newString.length < s.length) newString else s
  }

}
