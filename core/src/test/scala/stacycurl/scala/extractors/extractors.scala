package stacycurl.scala.extractors

import org.junit.Test
import scala.util._
import scalaz.{Lens, Monoid, Semigroup}

import org.junit.Assert._
import scalaz.std.list._
import scalaz.syntax.std.boolean._


class ExtractorsTests {
  @Test def canCreateFromFunction {
    assertEquals("Contains(foo)", ContainsFoo.describe)
    assertEquals(List("foo", "food", "unmatched"), List("foo", "food", "other").map {
      case ContainsFoo(s) => s
      case _              => "unmatched"
    })
  }

  @Test def canCreateFromPartialFunction {
    assertEquals("Partial", ContainsBar.describe)
    assertEquals("Partial(id)", Extractor.from[String].pf({ case s => s }, "id").describe)

    assertEquals(List("bar", "bard", "unmatched"), List("bar", "bard", "other").map {
      case ContainsBar(s) => s
      case _              => "unmatched"
    })
  }

  @Test def canMapOverResult {
    val ReversedResult: Extractor[String, String] = ContainsFoo.map(_.reverse)

    assertEquals("Contains(foo).map", ReversedResult.describe)
    assertEquals("Contains(foo).map(reverse)", ContainsFoo.map(_.reverse, "reverse").describe)

    assertEquals(List("oof", "doof", "unmatched"), List("foo", "food", "other").map {
      case ReversedResult(r) => r
      case _                 => "unmatched"
    })
  }

  @Test def canCollectOverResult {
    val LengthGT3 = Length.collect[String] {
      case l if l > 3 => "> 3"
    }

    assertEquals("Function.collect", LengthGT3.describe)
    assertEquals("Function.collect(name)", Length.collect[Int]({ case i => i }, "name").describe)

    assertEquals(List("> 3", "unmatched"), List("abcd", "abc").map {
      case LengthGT3(message) => message
      case _                  => "unmatched"
    })
  }

  @Test def canFlatMapOverResult {
    val Contains     = ContainsFoo.map(_ => "bar").orElse(Extractor.string.contains("oof").map(_ => "rab"))
    val ContainsBoth = Contains.flatMap(value => Extractor.string.contains(value))

    assertEquals("First(Contains(foo).map, Contains(oof).map).flatMap", ContainsBoth.describe)
    assertEquals("Never.flatMap(name)",
      Extractor.never[Int, Int].flatMap(_ => Extractor.never[Int, Int], "name").describe)

    assertEquals(List(true, true, false, false, false, false),
      List("foobar", "raboof", "foo", "bar", "rab", "oof").map {
        case ContainsBoth(_) => true
        case _               => false
      }
    )
  }

  @Test def canContramapOverInput {
    val ReversedInput = ContainsBar.contramap[String](_.reverse)

    assertEquals("Partial.contramap", ReversedInput.describe)
    assertEquals("Partial.contramap(reverse)", ContainsBar.contramap[String](_.reverse, "reverse").describe)

    assertEquals(List("bar", "bard", "unmatched"), List("rab", "drab", "other").map {
      case ReversedInput(s) => s
      case _                => "unmatched"
    })
  }

  @Test def map {
    assertEquals("Function", Length.describe)
    assertEquals("Function(length)", Extractor.map[String](_.length, "length").describe)

    assertEquals(List(1, 3), List("a", "foo").map {
      case Length(i) => i
    })
  }

  @Test def when {
    assertEquals("When", IsThree.describe)
    assertEquals("When(== 3)", Extractor.when[Int](_ == 3, "== 3").describe)

    assertEquals(List(true, false), List(3, 4).map {
      case IsThree(_) => true
      case _          => false
    })
  }

  @Test def unless {
    val NotThree = Extractor.unless[Int](_ == 3)

    assertEquals("Unless", NotThree.describe)
    assertEquals("Unless(== 3)", Extractor.unless[Int](_ == 3, "== 3").describe)

    assertEquals(List(false, true), List(3, 4).map {
      case NotThree(_) => true
      case _           => false
    })
  }

  @Test def canCompose {
    val LengthThree = IsThree.compose(Length)

    assertEquals("When.compose(Function)", LengthThree.describe)
    assertEquals(List(true, true, false), List("foo", "bar", "other").map {
      case LengthThree(_) => true
      case _              => false
    })
  }

  @Test def canComposeWithAndThen {
    val LengthThree = Length.andThen(IsThree)

    assertEquals("Function.andThen(When)", LengthThree.describe)
    assertEquals(List(true, true, false), List("foo", "bar", "other").map {
      case LengthThree(_) => true
      case _              => false
    })
  }

  @Test def canCreateFromMap {
    val FromMap = Extractor.fromMap(Map("foo" -> 1, "bar" -> 2))

    assertEquals("FromMap(size = 2)", FromMap.describe)
    assertEquals(List(1, 2, -1), List("foo", "bar", "other").map {
      case FromMap(result) => result
      case _               => -1
    })
  }

  @Test def canComposeWithAlternative {
    val ContainsFooOrBar = Extractor.never[String, String].orElse(
      ContainsFoo.map(_ => "foo")).orElse(ContainsBar.map(_ => "bar"))

    assertEquals("First(Never, Contains(foo).map, Partial.map)", ContainsFooOrBar.describe)
    assertEquals(List("foo", "foo", "bar", "unmatched"), List("foobar", "foo", "bar", "other").map {
      case ContainsFooOrBar(sub) => sub
      case _                   => "unmatched"
    })
  }

  @Test def canComposeWithLast {
    val ContainsFooOrBar = Extractor.never[String, String].last(
      ContainsFoo.map(_ => "foo")).last( ContainsBar.map(_ => "bar"))

    assertEquals("Last(Never, Contains(foo).map, Partial.map)", ContainsFooOrBar.describe)
    assertEquals(List("bar", "foo", "bar", "unmatched"), List("foobar", "foo", "bar", "other").map {
      case ContainsFooOrBar(sub) => sub
      case _                     => "unmatched"
    })
  }

  @Test def canComposeWithAppend {
    implicit val firstString: Semigroup[String] = Semigroup.firstSemigroup[String]

    val ContainsFooOrBar = ContainsFoo.map(_ => "foo").append(ContainsBar.map(_ => "bar"))

    assertEquals("Contains(foo).map.append(Partial.map)", ContainsFooOrBar.describe)
    assertEquals(List("foo", "foo", "bar", "unmatched"), List("foobar", "foo", "bar", "other").map {
      case ContainsFooOrBar(sub) => sub
      case _                     => "unmatched"
    })
  }

  @Test def isFunction {
    assertEquals(List(Some("foo"), None), List("foo", "bar").map(Extractor.string.contains("foo")))
  }

  @Test def canFallbackUsingGetOrElse {
    val ToInt = Extractor.fromMap("foo" -> 1, "bar" -> 2).getOrElse(3)

    assertEquals("FromMap(size = 2).getOrElse(3)", ToInt.describe)
    assertEquals(List(1, 2, 3), List("foo", "bar", "other").map {
      case ToInt(result) => result
    })
  }

  @Test def canChooseExceptionToFailWith {
    case class Unknown(message: String) extends Exception(message) {
      override def toString = message
    }

    val ToInt = Extractor.fromMap("foo" -> 1).orThrow(Unknown("unknown"))
    val DoubleMap = Extractor.fromMap("foo" -> 1.0)
    val ToDouble = DoubleMap.orThrow((s: String) => Unknown("unknown: " + s))

    assertEquals("FromMap(size = 1).orThrow(unknown)", ToInt.describe)
    assertEquals("FromMap(size = 1).orThrow", ToDouble.describe)
    assertEquals("FromMap(size = 1).orThrow(unknown)",
      DoubleMap.orThrow((s: String) => Unknown("u"), "unknown").describe)

    assertEquals(List(Success(1), Failure(Unknown("unknown"))), List("foo", "other").map(item => {
      Try(item match { case ToInt(i) => i })
    }))

    assertEquals(List(Success(1.0), Failure(Unknown("unknown: other"))), List("foo", "other").map(item => {
      Try(item match { case ToDouble(d) => d })
    }))
  }

  @Test def canFilterResult {
    val ContainsFoo = this.ContainsFoo.filter(_.length > 3)

    assertEquals("Contains(foo).filter", ContainsFoo.describe)
    assertEquals("Contains(foo).filter(name)", this.ContainsFoo.filter(_.length > 3, "name").describe)

    assertEquals(List("unknown", "food", "unknown"), List("foo", "food", "other").map {
      case ContainsFoo(f) => f
      case _              => "unknown"
    })
  }

  @Test def canZip {
    val GreaterThan3 = Extractor.when[Int](_ > 3)
    val FooGT3 = ContainsFoo.zip(GreaterThan3)

    assertEquals("Contains(foo).zip(When)", FooGT3.describe)
    assertEquals(List(true, false, false, false), List(("foo", 4), ("foo", 0), ("bar", 4), ("bar", 0)).map {
      case FooGT3(_) => true
      case _         => false
    })
  }

  @Test def unzip {
    val TupleOfLists: Extractor[List[(Int, String)], (List[Int], List[String])] =
      Extractor.unzip[Int, String, List]

    assertEquals("Unzip", TupleOfLists.describe)
    assertEquals((List(1, 2), List("one", "two")), List((1, "one"), (2, "two")) match {
      case TupleOfLists(ints, strings) => (ints, strings)
    })
  }

  @Test def lens {
    val Is1One = Extractor.when[(Int, String)](_ == (1, "one"))
    val FirstIs1 = Is1One.lens(Lens.firstLens[Int, String])

    assertEquals("When.lens", FirstIs1.describe)
    assertEquals("When.lens(first)", Is1One.lens(Lens.firstLens[Int, String], "first").describe)
    assertEquals(List(true, false), List((1, "one"), (2, "two")).map {
      case FirstIs1(_) => true
      case _           => false
    })
  }

  @Test def regex {
    val Matches = Extractor.string.regex("(.*):(.*)") {
      case List(first, second) => (first, second)
    }

    assertEquals("Regex((.*):(.*))", Matches.describe)
    assertEquals(List(("abc", "def"), ("", "def"), ("abc", ""), ("?", "?")),
      List("abc:def", ":def", "abc:", "blah").map {
        case Matches(first, second) => (first, second)
        case _                      => ("?", "?")
      }
    )
  }

  @Test def liftToOption {
    val OptionContainsFoo = ContainsFoo.liftToOption

    assertEquals("Contains(foo).liftToOption", OptionContainsFoo.describe)
    assertEquals(List(true, false, false), List(Some("foo"), Some("bar"), None).map {
      case OptionContainsFoo(_) => true
      case _                    => false
    })
  }

  @Test def forall {
    val AllContainsFoo = ContainsFoo.forall[List]

    assertEquals("Contains(foo).forAll[List]", AllContainsFoo.describe)
    assertEquals(List(true, true, true, false, false),
      List(List("foo"), List("food", "foo"), Nil, List("bar"), List("foo", "bar")).map {
        case AllContainsFoo(_) => true
        case _                 => false
      }
    )
  }

  @Test def exists {
    val ExistsContainsFoo = ContainsFoo.exists[List]

    assertEquals("Contains(foo).exists[List]", ExistsContainsFoo.describe)
    assertEquals(List(true, true, true, false, false),
      List(List("foo"), List("bar", "foo"), Nil, List("bar"), List("bar", "barf")).map {
        case ExistsContainsFoo(_) => true
        case _                    => false
      }
    )
  }

  @Test def pf {
    val Mapped = Extractor.fromMap(1 -> 2, 2 -> 3)

    assertEquals(List(2, 3), List(1, 2).collect(Mapped.pf))

    assertSame(Mapped.pf, Extractor.from[Int].pf(Mapped.pf).pf)
  }

  @Test def point {
    val Point = Extractor.point[String, Int](Some(1))

    assertEquals("Point(Some(1))", Point.describe)
    assertEquals(List(1), List("blah").map {
      case Point(i) => i
    })
  }

  @Test def id {
    val Id = Extractor.id[Int]

    assertEquals("Id", Id.describe)
    assertEquals(List(1, 2), List(1, 2).map {
      case Id(i) => i
    })
  }

  @Test def apply {
    val Apply = Extractor.apply[String, Int]((s: String) => s.contains("foo").option(3))

    assertEquals("Apply", Apply.describe)
    assertEquals("Apply(name)", Extractor.apply[String, String](Some(_), "name").describe)
    assertEquals("Apply(name)", Extractor.from[String](Some(_), "name").describe)

    assertEquals(List(3, 0), List("foo", "bar").map {
      case Apply(i) => i
      case _        => 0
    })
  }

  @Test def first {
    val FirstContainsFoo = ContainsFoo.arrFirst[Int]

    assertEquals("ArrFirst(Contains(foo))", FirstContainsFoo.describe)
    assertEquals(List(true, false), List(("foo", 1), ("bar", 2)).map {
      case FirstContainsFoo(_) => true
      case _                   => false
    })
  }

  @Test def canName {
    assertEquals("Function(Length)", Length.named("Length").describe)
  }

  private lazy val ContainsFoo = Extractor.string.contains("foo")

  private lazy val ContainsBar = Extractor.from[String].pf {
    case s if s.contains("bar") => s
  }

  private lazy val Length: Extractor[String, Int] = Extractor.map(_.length)
  private lazy val IsThree: Extractor[Int, Int] = Extractor.when[Int](_ == 3)
}
