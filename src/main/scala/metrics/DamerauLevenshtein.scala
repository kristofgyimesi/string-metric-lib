package metrics

import scala.collection.immutable.HashMap
import utils.ConversionUtils._

object DamerauLevenshtein {

  /** Computes the Damerau-Levenshtein distance between two strings.
    * The operation costs can be parameterized.
    *
    * @param str1 The first string.
    * @param str2 The second string.
    * @param deletionCosts A HashMap that sets the cost for `deleting` specific characters.
    *                      Each key is a character, and the value is the cost of deleting that character.
    *                       Defaults to an empty HashMap.
    * @param insertionCosts A HashMap that sets the cost for `inserting` specific characters.
    *                       Each key is a character, and the value is the cost of inserting that character.
    *                        Defaults to an empty HashMap.
    * @param substitutionCosts A HashMap that sets the cost for `substituting` specific characters.
    *                          Each key is a tuple of characters, and the value is the cost of substituting the first
    *                          character for the second character.
    *                           Defaults to an empty HashMap.
    * @param transpositionCosts A HashMap that sets the cost for `transposing` specific characters.
    *                           Each key is a tuple of characters, and the value is the cost of transposing the first
    *                           character for the second character, if they are next to each other.
    *                            Defaults to an empty HashMap.
    * @return The Damerau-Levenshtein distance between `str1` and `str2`
    */
  def distance(
      str1: String,
      str2: String,
      deletionCosts: HashMap[Char, Double] = null,
      insertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null,
      transpositionCosts: HashMap[(Char, Char), Double] = null
  ): Double =
    calcDistance(
      s1 = Option(str1).getOrElse(""),
      s2 = Option(str2).getOrElse(""),
      deletionCosts = Option(deletionCosts).getOrElse(new HashMap[Char, Double]),
      insertionCosts = Option(insertionCosts).getOrElse(new HashMap[Char, Double]),
      substitutionCosts = Option(substitutionCosts).getOrElse(new HashMap[(Char, Char), Double]),
      transpositionCosts = Option(transpositionCosts).getOrElse(new HashMap[(Char, Char), Double])
    )

  private def calcDistance(
      s1: String,
      s2: String,
      deletionCosts: HashMap[Char, Double] = null,
      insertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null,
      transpositionCosts: HashMap[(Char, Char), Double] = null
  ): Double = {
    val len1 = s1.length
    val len2 = s2.length

    val initialRow = Vector.range(0, len2 + 1).map(x => x.toDouble)
    val finalRow = (1 to len1).foldLeft(initialRow, initialRow) { case ((twoRowsBack, previousRow), i) =>
      val s1CurrChar = s1(i - 1)
      val currentRow = new Array[Double](len2 + 1)
      currentRow(0) = i.toDouble

      (1 to len2).foreach { j =>
        val s2CurrChar = s2(j - 1)

        if (s1CurrChar == s2CurrChar) {
          currentRow(j) = previousRow(j - 1)
        } else {
          currentRow(j) = Seq(
            previousRow(j) + deletionCosts.getOrElse(s1CurrChar, 1.0),
            currentRow(j - 1) + insertionCosts.getOrElse(s2CurrChar, 1.0),
            previousRow(j - 1) + substitutionCosts.getOrElse((s1CurrChar, s2CurrChar), 1.0)
          ).min

          if (i > 1 && j > 1 && s1CurrChar == s2(j - 2) && s1(i - 2) == s2CurrChar) {
            val transCost = transpositionCosts.getOrElse((s1(i - 2), s1CurrChar), 1.0)
            currentRow(i) = math.min(currentRow(i), twoRowsBack(i - 2) + transCost)
          }
        }
      }

      (previousRow, currentRow.toVector)
    }

    finalRow._2(len2)
  }

  /** Computes normalized Damerau-Levenshtein string similarity value between two strings.
    * It calculates the value for each parameter order that the input strings can be passed to the function.
    * This value is between 0 and 1.
    *
    * @param str1              The first string
    * @param str2              The second string
    * @param deletionCosts     A HashMap that sets the cost for `deleting` specific characters.
    *                          Each key is a character, and the value is the cost of deleting that character.
    * @param insertionCosts    A HashMap that sets the cost for `inserting` specific characters.
    *                          Each key is a character, and the value is the cost of inserting that character.
    * @param substitutionCosts A HashMap that sets the cost for `substituting` specific characters.
    *                          Each key is a tuple of characters, and the value is the cost of substituting the first
    *                          character for the second character.
    * @return The normalized Levenshtein string similarity between `str1` and `str2`.
    */
  def similarity(
      str1: String,
      str2: String,
      deletionCosts: HashMap[Char, Double] = null,
      insertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null,
      transpositionCosts: HashMap[(Char, Char), Double] = null
  ): Double = {
    val s1 = Option(str1).getOrElse("")
    val s2 = Option(str2).getOrElse("")

    val maxLength = Seq(s1.length, s2.length).max
    val distances = Seq((str1, str2), (str2, str1)).map { case (s1, s2) =>
      distance(s1, s2, deletionCosts, insertionCosts, substitutionCosts, transpositionCosts)
    }

    if (maxLength == 0) {
      1.0
    } else {
      1 - (distances.min / maxLength)
    }
  }
}
