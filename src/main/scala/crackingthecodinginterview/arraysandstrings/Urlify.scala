package crackingthecodinginterview.arraysandstrings

object Urlify {

  def urlify(chars: Array[Char], trueLength: Int): Unit = {
    // O(n) - could do marginally more efficiently (?) in terms of constant factors
    // by walking backwards through the array and starting to count spaces after hitting a real char
    // but it wouldn't change the big-O
    var remainingSpaceCount = chars.take(trueLength).count(_ == ' ')
    // O(n) (nothing within the loop takes anything more than constant time)
    (trueLength - 1 to 0 by -1).foreach { i =>
      val offset = i + (remainingSpaceCount * 2)
      if (chars(i) == ' ') {
        chars(offset) = '0'
        chars(offset - 1) = '2'
        chars(offset - 2) = '%'
        remainingSpaceCount -= 1
      }
      else {
        chars(offset) = chars(i)
      }
    }
  }

}
