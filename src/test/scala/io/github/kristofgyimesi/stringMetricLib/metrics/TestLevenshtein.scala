package io.github.kristofgyimesi.stringMetricLib.metrics

import io.github.kristofgyimesi.stringMetricLib.metrics.Levenshtein._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop._

import scala.collection.immutable.HashMap
class TestLevenshtein extends AnyFunSpec with Matchers {

  describe("Levenshtein distance") {
    it("should calculate original Levenshtein distance") {
      //Arrange
      val inputs: TableFor3[String, String, Double] = Table(
        ("string1", "string2", "expectedDist"),
        ("", "empty", 5.0),
        ("", "", 0.0),
        (null, null, 0.0),
        ("some", null, 4.0),
        (null, "some", 4.0),
        ("same", "same", 0.0),
        ("a", "ab", 1.0),              // One insertion
        ("ab", "a", 1.0),              // One deletion
        ("ab", "ac", 1.0),             // One substitution
        ("elephant", "relevant", 3.0), // One of each operation
        ("hello", "hlelo", 2.0)        // Two substitutions
      )

      // Act and assert
      forEvery(inputs) { (string1, string2, expectedDist) =>
        distance(string1, string2) should equal(expectedDist)
      }
    }

    it("should calculate parameterized Levenshtein distance") {
      //Arrange
      val inputs =
        Table(
          ("string1", "string2", "delCosts", "insCosts", "substCosts", "expectedDist"),
          ("a", "ab", null, HashMap('b' -> 2.0), null, 2.0), // Insert 'b'
          ("ab", "a", HashMap('b' -> 2.0), null, null, 2.0), // Delete 'b'
          ("ab", "ac", null, null, HashMap(('b', 'c') -> 0.5), 0.5), // Substitute 'b' for 'c'
          ("ab", "ac", null, null, HashMap(('b', 'c') -> 3.0), 2.0), // Delete 'b' and then insert 'c'
          (
            "elephant",
            "relevant",
            HashMap('h'        -> 0.5),
            HashMap('r'        -> 0.5),
            HashMap(('p', 'v') -> 0.75),
            2.25
          ) // Insert 'r', replace 'p' for 'v', then delete 'h'
        )

      // Act and assert
      forEvery(inputs) { (string1, string2, delCosts, insCosts, substCosts, expectedDist) =>
        distance(string1, string2, delCosts, insCosts, substCosts) should equal(expectedDist)
      }
    }
  }

  describe("Levenshtein similarity") {
    it("should calculate the similarity value") {
      //Arrange
      val inputs =
        Table(
          ("string1", "string2", "delCosts", "insCosts", "substCosts", "expectedDist"),
          (null, null, null, null, null, 1.0),
          ("", "empty", null, null, null, 0.0),
          ("", "", null, null, null, 1.0),
          ("same", "same", null, null, null, 1.0),
          ("a", "b", null, null, HashMap(('a', 'b') -> 1.0), 0.0),
          ("a", "ab", HashMap('b' -> 0.5), HashMap('b' -> 1.0), null, 0.75), // Use lower distance: delete 'b'
          ("ab", "ba", null, null, null, 0.0), // delete 'a' then insert 'b'
          ("cat", "cart", HashMap('r' -> 0.5), null, null, 0.875), // should be the same either way
          ("cart", "cat", HashMap('r' -> 0.5), null, null, 0.875), // should be the same either way
          ("book", "back", null, null, null, 0.5)
        )

      // Act and assert
      forEvery(inputs) { (string1, string2, delCosts, insCosts, substCosts, expectedDist) =>
        similarity(string1, string2, delCosts, insCosts, substCosts) should equal(expectedDist)
      }
    }
  }

  describe("Levenshtein symmetric similarity") {
    it("should calculate the similarity value with symmetric cost maps") {
      //Arrange
      val inputs =
        Table(
          ("string1", "string2", "delAndInsCosts", "substCosts", "expectedDist"),
          (null, null, null, null, 1.0),
          ("", "empty", null, null, 0.0),
          ("", "", null, null, 1.0),
          ("same", "same", null, null, 1.0),
          ("a", "b", null, HashMap(('a', 'b') -> 1.0), 0.0),
          ("a", "ab", HashMap('b' -> 0.5), null, 0.75), // insert 'b'
          ("ab", "a", HashMap('b' -> 0.5), null, 0.75), // delete 'b'
          ("ab", "ba", null, null, 0.0), // Use lower distance: transpose 'b' to 'a'
          ("cat", "cart", HashMap('r' -> 0.5), null, 0.875), // should be the same either way
          ("cart", "cat", HashMap('r' -> 0.5), null, 0.875), // should be the same either way
          ("book", "back", null, null, 0.5)
        )

      // Act and assert
      forEvery(inputs) { (string1, string2, delAndInsCosts, substCosts, expectedDist) =>
        symmetricSimilarity(string1, string2, delAndInsCosts, substCosts) should equal(expectedDist)
      }
    }
  }
}
