package stacycurl.scala.extractors

import org.junit.Test

import org.junit.Assert._
import scalaz.syntax.std.boolean._


class ExtractorsTests {
  @Test def canCreateFromFunction {
    val ContainsFoo = Extractor.from[String](s => s.contains("foo").option(s))

    val foo = PartialFunction.condOpt("foo") {
      case ContainsFoo(s) => s
    }

    assertEquals(Some("foo"), foo)

    val food = PartialFunction.condOpt("food") {
      case ContainsFoo(s) => s
    }

    assertEquals(Some("food"), food)
  }
}
