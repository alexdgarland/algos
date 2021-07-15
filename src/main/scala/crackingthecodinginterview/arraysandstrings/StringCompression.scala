package crackingthecodinginterview.arraysandstrings

object StringCompression {

  /***
   * Shared code to run through a string (in O(n)),
   * calling a given function with the character and run-length whenever the end of a run of the same char is reached.
   *
   * @param s
   * @param function
   */
  private def executeOverCharacterRuns(s: String, function: (Char, Int) => Unit): Unit = {
    var activeChar: Option[Char] = None
    var currentSeriesLength: Int = 0

    val applyFunction = { () => activeChar.foreach { charValue => function(charValue, currentSeriesLength) } }

    s.foreach { char =>
      if(activeChar.contains(char)) {
        currentSeriesLength += 1
      }
      else {
        applyFunction()
        activeChar = Some(char)
        currentSeriesLength = 1
      }
    }
    applyFunction()
  }

  private def runLengthSaving(seriesLength: Int) = seriesLength - (1 + seriesLength.toString.length)

  /***
   * Compress string by run-length encoding, i.e. aaaaabbbcccccccddd resolves to a5b3c7d3.
   * Return the original string if the compression would not reduce overall length.
   *
   * This version of the function is optimised for time complexity only,
   * running in O(n) as it passes through the string once and uses a StringBuilder.
   *
   * See below for alternative optimisations targeting the case where compression cannot reduce length -
   * unless this case is very likely (or the typical savings when it does occur are known to be very significant),
   * this implementation may be the best option due to simplicity if nothing else.
   *
   * @param s
   * @return
   */
  def compressString(s: String): String = {
    val builder = new StringBuilder()
    executeOverCharacterRuns(s, (char: Char, seriesLength: Int) => builder.append(s"$char$seriesLength"))
    val newString = builder.toString()
    if(newString.length < s.length) newString else s
  }

  /***
   *  Alternative version which avoids building the final string if not needed.
   *  This doesn't have many downsides - just tracking the net saving as an int - and can save space in the non-compressible case.
   *
   *  No change to time complexity (O(n)).
   *
   * @param s
   * @return
   */
  def compressStringAvoidingNewStringIfNotNeeded(s: String): String = {
    val builder = new StringBuilder()
    var netSaving: Int = 0
    executeOverCharacterRuns(
      s,
      (char, seriesLength) => {
        builder.append(s"$char$seriesLength")
        netSaving += runLengthSaving(seriesLength)
      }
    )
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
    var netSaving: Int = 0
    executeOverCharacterRuns(s, (_, seriesLength) => netSaving += runLengthSaving(seriesLength))
    // If we know there will be no saving, return original string without allocating StringBuilder
    if (netSaving <= 0) { return s }
    // Else run through the loop again using a builder
    val builder = new StringBuilder()
    executeOverCharacterRuns(s, (char, seriesLength) => builder.append(s"$char$seriesLength"))
    builder.toString()
  }

}
