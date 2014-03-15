package stacycurl.scala.extractors

import org.junit.Test

import org.junit.Assert._
import scalaz.syntax.std.boolean._


class ExtractorsTests {
  @Test def canCreateFromFunction {
    val ContainsFoo = Extractor.from[String](s => s.contains("foo").option(s))

    assertEquals(List("foo", "food", "unmatched"), List("foo", "food", "other").map {
      case ContainsFoo(s) => s
      case _              => "unmatched"
    })
  }

  @Test def canCreateFromPartialFunction {
    val ContainsBar = Extractor.from[String].pf {
      case s if s.contains("bar") => s
    }

    assertEquals(List("bar", "bard", "unmatched"), List("bar", "bard", "other").map {
      case ContainsBar(s) => s
      case _              => "unmatched"
    })
  }

  @Test def canMapOverResult {
    val ContainsFoo: Extractor[String, String]    = Extractor.from[String](s => s.contains("foo").option(s))
    val ReversedResult: Extractor[String, String] = ContainsFoo.map(_.reverse)

    assertEquals(List("oof", "doof", "unmatched"), List("foo", "food", "other").map {
      case ReversedResult(r) => r
      case _                 => "unmatched"
    })
  }

  @Test def canContramapOverInput {
    val ContainsBar = Extractor.from[String].pf {
      case s if s.contains("bar") => s
    }

    val ReversedInput = ContainsBar.contramap[String](_.reverse)

    assertEquals(List("bar", "bard", "unmatched"), List("rab", "drab", "other").map {
      case ReversedInput(s) => s
      case _                => "unmatched"
    })
  }
}
