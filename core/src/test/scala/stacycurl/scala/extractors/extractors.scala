package stacycurl.scala.extractors

import org.junit.Test
import scala.util._

import org.junit.Assert._
import scalaz.std.list._
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

  @Test def canComposeWithAlternative {
    val ContainsFooOrBar = Extractor.string.contains("foo").orElse(Extractor.string.contains("bar"))

    assertEquals(List(true, true, true, false), List("foobar", "foo", "bar", "other").map {
      case ContainsFooOrBar(_) => true
      case _                   => false
    })
  }

  @Test def isFunction {
    assertEquals(List(Some("foo"), None), List("foo", "bar").map(Extractor.string.contains("foo")))
  }

  @Test def canFallbackUsingGetOrElse {
    val ToInt = Extractor.fromMap("foo" -> 1, "bar" -> 2).getOrElse(3)

    assertEquals(List(1, 2, 3), List("foo", "bar", "other").map {
      case ToInt(result) => result
    })
  }

  @Test def canChooseExceptionToFailWith {
    case class Unknown(message: String) extends Exception(message)
    val ToInt = Extractor.fromMap("foo" -> 1).orThrow(Unknown("unknown"))

    assertEquals(List(Success(1), Failure(Unknown("unknown"))), List("foo", "other").map(item => {
      Try(item match { case ToInt(i) => i })
    }))

    val ToDouble = Extractor.fromMap("foo" -> 1.0).orThrow((s: String) => Unknown("unknown: " + s))

    assertEquals(List(Success(1.0), Failure(Unknown("unknown: other"))), List("foo", "other").map(item => {
      Try(item match { case ToDouble(d) => d })
    }))
  }

  @Test def canFilterResult {
    val ContainsFoo = Extractor.string.contains("foo").filter(_.length > 3)

    assertEquals(List("unknown", "food", "unknown"), List("foo", "food", "other").map {
      case ContainsFoo(f) => f
      case _              => "unknown"
    })
  }

  @Test def canZip {
    val ContainsFoo  = Extractor.string.contains("foo")
    val GreaterThan3 = Extractor.when[Int](_ > 3)
    val FooGT3 = ContainsFoo.zip(GreaterThan3)

    assertEquals(List(true, false, false, false), List(("foo", 4), ("foo", 0), ("bar", 4), ("bar", 0)).map {
      case FooGT3(_) => true
      case _         => false
    })
  }

  @Test def unzip {
    val TupleOfLists: Extractor[List[(Int, String)], (List[Int], List[String])] =
      Extractor.unzip[Int, String, List]

    assertEquals((List(1, 2), List("one", "two")), List((1, "one"), (2, "two")) match {
      case TupleOfLists(ints, strings) => (ints, strings)
    })
  }
}
