package stacycurl.scala.extractors

import org.junit.Test

import org.junit.Assert._
import scalaz.syntax.std.boolean._


class ExtractorsTests {
  @Test def canCreateFromFunction {
    val ContainsFoo = Extractor.string.contains("foo")

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
    val ReversedResult: Extractor[String, String] = Extractor.string.contains("foo").map(_.reverse)

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
    val Length: Extractor[String, Int] = Extractor.map(_.length)
    val IsThree: Extractor[Int, Int] = Extractor.when[Int](_ == 3)
    val LengthThree = IsThree.compose(Length)

    assertEquals(List(true, true, false), List("foo", "bar", "other").map {
      case LengthThree(_) => true
      case _              => false
    })
  }

  @Test def canComposeWithAndThen {
    val Length  = Extractor.map[String](_.length)
    val IsThree = Extractor.when[Int](_ == 3)
    val LengthThree = Length.andThen(IsThree)

    assertEquals(List(true, true, false), List("foo", "bar", "other").map {
      case LengthThree(_) => true
      case _              => false
    })
  }

  @Test def canCreateFromMap {
    val FromMap = Extractor.fromMap(Map("foo" -> 1, "bar" -> 2))

    assertEquals(List(1, 2, -1), List("foo", "bar", "other").map {
      case FromMap(result) => result
      case _               => -1
    })
  }
}
