package metrics

import metrics.DamerauLevenshtein._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop._

import scala.collection.immutable.HashMap
class TestDamerauLevenshtein extends AnyFunSpec with Matchers {

  describe("Levenshtein distance") {
    it("should calculate original Damerau-Levenshtein distance") {
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
        ("ab", "ba", 1.0),             // One transposition
        ("elephant", "relevant", 3.0), // One of each operation
        ("hello", "hlelo", 1.0)        // One transposition
      )

      // Act and assert
      forEvery(inputs) { (string1, string2, expectedDist) =>
        distance(string1, string2) should equal(expectedDist)
      }
    }

    it("should calculate parameterized Damerau-Levenshtein distance") {
      //Arrange
      val inputs =
        Table(
          ("string1", "string2", "delCosts", "insCosts", "substCosts", "transCosts", "expectedDist"),
          ("a", "ab", null, HashMap('b' -> 2.0), null, null, 2.0), // Insert 'b'
          ("ab", "a", HashMap('b' -> 2.0), null, null, null, 2.0), // Delete 'b'
          ("ab", "ac", null, null, HashMap(('b', 'c') -> 0.5), null, 0.5), // Substitute 'b' for 'c'
          ("ab", "ac", null, null, HashMap(('b', 'c') -> 3.0), null, 2.0), // Delete 'b' and then insert 'c'
          (
            "elephant",
            "relevant",
            HashMap('h'        -> 0.5),
            HashMap('r'        -> 0.5),
            HashMap(('p', 'v') -> 0.75),
            null,
            2.25
          ), // Insert 'r', replace 'p' for 'v', then delete 'h'
          ("hello", "hlelo", null, null, null, HashMap(('e', 'l') -> 0.5), 0.5) // Transpose 'e' to 'l'
        )

      // Act and assert
      forEvery(inputs) { (string1, string2, delCosts, insCosts, substCosts, transCost, expectedDist) =>
        distance(string1, string2, delCosts, insCosts, substCosts, transCost) should equal(expectedDist)
      }
    }
  }

  describe("Levenshtein similarity") {
    it("should calculate the similarity value") {
      //Arrange
      val inputs =
        Table(
          ("string1", "string2", "delCosts", "insCosts", "substCosts", "transCosts", "expectedDist"),
          (null, null, null, null, null, null, 1.0),
          ("", "empty", null, null, null, null, 0.0),
          ("", "", null, null, null, null, 1.0),
          ("same", "same", null, null, null, null, 1.0),
          ("a", "b", null, null, HashMap(('a', 'b') -> 3.0), null, 0.0),
          ("a", "ab", null, HashMap('b' -> 2.0), null, null, 0.5), // Use lower distance: delete 'b'
          ("ab", "ba", null, null, null, HashMap(('a', 'b') -> 0.5), 0.75), // Use lower distance: transpose 'b' to 'a'
          ("cat", "cart", HashMap('r' -> 2.0), null, null, null, 0.75), // Use lower distance: insert `r`
          ("cats", "cat", HashMap('s' -> 2.0), HashMap('s' -> 2.0), null, null, 0.5),
          ("book", "back", null, null, null, null, 0.5)
        )

      // Act and assert
      forEvery(inputs) { (string1, string2, delCosts, insCosts, substCosts, transCost, expectedDist) =>
        similarity(string1, string2, delCosts, insCosts, substCosts, transCost) should equal(expectedDist)
      }
    }
  }
}
