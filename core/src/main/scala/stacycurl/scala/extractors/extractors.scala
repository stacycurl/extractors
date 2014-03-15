package stacycurl.scala.extractors


object Extractor {
  def from[A] = new ExtractorBuilder[A]

  class ExtractorBuilder[A] {
    def apply[B](f: A => Option[B]): Extractor[A, B] = FunctionExtractor[A, B](f)
  }

  private case class FunctionExtractor[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }
}

trait Extractor[A, B] {
  def unapply(a: A): Option[B]
}
