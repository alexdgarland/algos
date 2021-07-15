package crackingthecodinginterview.arraysandstrings

object StringCompression {

  /***
   * Compress string by run-length encoding, i.e. aaaaabbbcccccccddd resolves to a5b3c7d3.
   * Return the original string if the compression would not reduce overall length.
   *
   * This version of the function is optimised for time complexity only,
   * running in O(n) as it passes through the string once and uses a StringBuilder.
   *
   * See below for alternative optimisations targeting the case where a new compressed string is not possible -
   * unless this case is very likely (or the typical savings when it does occur are known to be very significant),
   * this implementation may be the best option due to simplicity if nothing else.
   *
   * @param s
   * @return
   */
  def compressString(s: String): String = {
    val builder = new StringBuilder()
    var activeChar: Option[Char] = None
    var currentSeriesLength: Int = 0

    val flush = { () => activeChar.foreach { charValue => builder.append(s"$charValue$currentSeriesLength") } }

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

  /***
   *  Alternative version which avoids building the final string if not needed.
   *  This doesn't have many downsides - just tracking the net saving as an int - and can save space in a non-typical case.
   *
   *  No change to time complexity (O(n)).
   *
   * @param s
   * @return
   */
  def compressStringAvoidingNewStringIfNotNeeded(s: String): String = {
    val builder = new StringBuilder()
    var activeChar: Option[Char] = None
    var currentSeriesLength: Int = 0
    var netSaving: Int = 0

    def flush(): Unit = {
      activeChar.foreach { charValue =>
        builder.append(s"$charValue$currentSeriesLength")
        netSaving += currentSeriesLength - (1 + currentSeriesLength.toString.length)
      }
    }

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
    if(netSaving > 0) builder.toString() else s
  }

  /***
   * This alternative spends an extra iteration checking if compression is possible before using StringBuilder.
   *
   * Saves some further space in the non-compressible case, at the cost of a constant-factor increase in time for the compressible case.
   *
   * Still leaves time complexity unchanged at O(n)).
   *
   * @param s
   * @return
   */
  def compressStringAvoidingStringBuilderIfNotNeeded(s: String): String = {

    var activeChar: Option[Char] = None
    var currentSeriesLength: Int = 0
    var netSaving: Int = 0

    val incrementNetSaving = () => netSaving += currentSeriesLength - (1 + currentSeriesLength.toString.length)

    s.foreach { char =>
      if(activeChar.contains(char)) {
        currentSeriesLength += 1
      }
      else {
        incrementNetSaving()
        activeChar = Some(char)
        currentSeriesLength = 1
      }
    }
    incrementNetSaving()

    // If we know there will be no saving, return original string without allocating StringBuilder
    if (netSaving <= 0) { return s }

    // Else reset loop variables and create StringBuilder, then run through the loop again using the builder
    activeChar = None
    currentSeriesLength = 0
    val builder = new StringBuilder()

    val flush = { () => activeChar.foreach { charValue => builder.append(s"$charValue$currentSeriesLength") } }

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
    builder.toString()
  }

}
