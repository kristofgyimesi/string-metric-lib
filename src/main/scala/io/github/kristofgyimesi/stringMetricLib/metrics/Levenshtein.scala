package io.github.kristofgyimesi.stringMetricLib.metrics

import io.github.kristofgyimesi.stringMetricLib.utils.ConversionUtils._
import scala.collection.immutable.HashMap

object Levenshtein {

  /** Computes the Levenshtein distance between two strings.
    * The operation costs can be parameterized.
    *
    * @param str1 The first string.
    * @param str2 The second string.
    * @param deletionCosts A HashMap that sets the cost for `deleting` specific characters.
    *                      Each key is a character, and the value is the cost of deleting that character.
    *                      Defaults to an empty HashMap.
    * @param insertionCosts A HashMap that sets the cost for `inserting` specific characters.
    *                       Each key is a character, and the value is the cost of inserting that character.
    *                       Defaults to an empty HashMap.
    * @param substitutionCosts A HashMap that sets the cost for `substituting` specific characters.
    *                          Each key is a tuple of characters, and the value is the cost of substituting the first
    *                          character for the second character.
    *                          Defaults to an empty HashMap.
    * @return The Levenshtein distance between `str1` and `str2`
    */
  def distance(
      str1: String,
      str2: String,
      deletionCosts: HashMap[Char, Double] = null,
      insertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null
  ): Double =
    calcDistance(
      s1 = Option(str1).getOrElse(""),
      s2 = Option(str2).getOrElse(""),
      deletionCosts = Option(deletionCosts).getOrElse(new HashMap[Char, Double]),
      insertionCosts = Option(insertionCosts).getOrElse(new HashMap[Char, Double]),
      substitutionCosts = Option(substitutionCosts).getOrElse(new HashMap[(Char, Char), Double])
    )

  private def calcDistance(
      s1: String,
      s2: String,
      deletionCosts: HashMap[Char, Double],
      insertionCosts: HashMap[Char, Double],
      substitutionCosts: HashMap[(Char, Char), Double]
  ): Double = {
    val len1 = s1.length
    val len2 = s2.length

    val initialRow = Vector.range(0, len2 + 1).map(x => x.toDouble)
    val finalRow = (1 to len1).foldLeft(initialRow) { (previousRow, i) =>
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
        }
      }

      currentRow.toVector
    }

    finalRow(len2)
  }

  /** Computes normalized Levenshtein string similarity value between two strings.
    * The operation costs can be parameterized.
    * It calculates the distance for both ways, since the costs configurations could be non-symmetric.
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
      substitutionCosts: HashMap[(Char, Char), Double] = null
  ): Double = {
    val s1 = Option(str1).getOrElse("")
    val s2 = Option(str2).getOrElse("")

    val maxLength = Seq(s1.length, s2.length).max
    val distances = Seq((str1, str2), (str2, str1)).map { case (s1, s2) =>
      distance(s1, s2, deletionCosts, insertionCosts, substitutionCosts)
    }

    if (maxLength == 0) {
      1.0
    } else {
      1 - (distances.min / maxLength)
    }
  }

    /** Computes the Levenshtein distance between two strings based on symmetric cost maps.
    * The operation costs can be parameterized.
    *
    * @param str1 The first string.
    * @param str2 The second string.
    * @param deletionAndInsertionCosts A HashMap that sets the cost for `deleting` specific characters.
    *                                  Each key is a character, and the value is the cost of deleting that character.
    *                                  Defaults to an empty HashMap. The values should be set between 0.0 and 1.0.
    * @param substitutionCosts A HashMap that sets the cost for `substituting` specific characters.
    *                          Each key is a tuple of characters, and the value is the cost of substituting the first
    *                          character for the second character. Defaults to an empty HashMap.
    *                          The values should be set between 0.0 and 1.0.
    * @param transpositionCosts A HashMap that sets the cost for `transposing` specific characters.
    *                           Each key is a tuple of characters, and the value is the cost of transposing the first
    *                           character for the second character, if they are next to each other.
    *                           Defaults to an empty HashMap. The values should be set between 0.0 and 1.0.
    * @return The Levenshtein distance between `str1` and `str2`
    */
  def symmetricDistance(
      str1: String,
      str2: String,
      deletionAndInsertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null
  ): Double =
    calcSymmetricDistance(
      s1 = Option(str1).getOrElse(""),
      s2 = Option(str2).getOrElse(""),
      deletionAndInsertionCosts = Option(deletionAndInsertionCosts).getOrElse(new HashMap[Char, Double]),
      substitutionCosts = Option(substitutionCosts).getOrElse(new HashMap[(Char, Char), Double])
    )

  private def calcSymmetricDistance(
      s1: String,
      s2: String,
      deletionAndInsertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null
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
            previousRow(j) + deletionAndInsertionCosts.getOrElse(s1CurrChar, 1.0),
            currentRow(j - 1) + deletionAndInsertionCosts.getOrElse(s2CurrChar, 1.0),
            previousRow(j - 1) + substitutionCosts
                                  .getOrElse((s1CurrChar, s2CurrChar),
                                    substitutionCosts.getOrElse((s2CurrChar, s1CurrChar), 1.0))
          ).min
        }
      }

      (previousRow, currentRow.toVector)
    }

    finalRow._2(len2)
  }

  /** Computes normalized Levenshtein string similarity value between two strings based on symmetric cost maps.
    * Because of this symmetry, it calculates the distance only one way.
    * This value is between 0 and 1.
    *
    * @param str1              The first string
    * @param str2              The second string
    * @param deletionAndInsertionCosts A HashMap that sets the cost for `deleting` specific characters.
    *                                  Each key is a character, and the value is the cost of deleting that character.
    *                                  The values should be set between 0.0 and 1.0.
    * @param substitutionCosts A HashMap that sets the cost for `substituting` specific characters.
    *                          Each key is a tuple of characters, and the value is the cost of
    *                          substituting the characters with each other.
    *                          The values should be set between 0.0 and 1.0.
    * @return The normalized Levenshtein string similarity between `str1` and `str2`.
    */
  def symmetricSimilarity(
      str1: String,
      str2: String,
      deletionAndInsertionCosts: HashMap[Char, Double] = null,
      substitutionCosts: HashMap[(Char, Char), Double] = null
  ): Double = {
    val s1 = Option(str1).getOrElse("")
    val s2 = Option(str2).getOrElse("")

    val maxLength = Seq(s1.length, s2.length).max
    val distance = symmetricDistance(s1, s2, deletionAndInsertionCosts, substitutionCosts)

    if (maxLength == 0) {
      1.0
    } else {
      1 - (distance / maxLength)
    }
  }
}
