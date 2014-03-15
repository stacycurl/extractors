package stacycurl.scala.extractors

import org.junit.Test

import org.junit.Assert._
import scalaz.syntax.std.boolean._


class ExtractorsTests {
  @Test def canCreateFromFunction {
    val ContainsFoo = contains("foo")

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
    val ReversedResult: Extractor[String, String] = contains("foo").map(_.reverse)

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

  @Test def canCompose {
    val Length: Extractor[String, Int] = Extractor.from[String](s => Option(s.length))
    val IsThree: Extractor[Int, Int] = Extractor.from[Int](i => (i == 3).option(i))
    val LengthThree = IsThree.compose(Length)

    assertEquals(List(true, true, false), List("foo", "bar", "other").map {
      case LengthThree(_) => true
      case _              => false
    })
  }

  private def contains(s: String) = Extractor.from[String](t => t.contains(s).option(t))
}
