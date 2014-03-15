package stacycurl.scala.extractors

import scalaz._


object Extractor extends ExtractorInstances {
  def from[A] = new ExtractorBuilder[A]

  class ExtractorBuilder[A] {
    def apply[B](f: A => Option[B]): Extractor[A, B] = FunctionExtractor[A, B](f)
    def pf[B](pf: PartialFunction[A, B]): Extractor[A, B] = apply(pf.lift)
  }

  private case class FunctionExtractor[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }

  private case class MappedExtractor[A, B, C](from: Extractor[A, B], f: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = from.unapply(a).map(f)
  }

  private case class ContravariantExtractor[A, B, C](from: Extractor[A, B], f: C => A) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = from.unapply(f(c))
  }
}

trait Extractor[A, B] {
  import Extractor._

  def unapply(a: A): Option[B]

  def map[C](f: B => C): Extractor[A, C] = MappedExtractor[A, B, C](this, f)
  def contramap[C](f: C => A): Extractor[C, B] = ContravariantExtractor[A, B, C](this, f)
}

trait ExtractorInstances {
  implicit def extractorFunctor[A]: Functor[({ type E[B] = Extractor[A, B] })#E] =
    new Functor[({ type E[B] = Extractor[A, B] })#E] {
      def map[B, C](eab: Extractor[A, B])(f: B => C): Extractor[A, C] = eab.map(f)
    }

  implicit def extractorContravariant[B]: Contravariant[({ type E[A] = Extractor[A, B] })#E] =
    new Contravariant[({ type E[A] = Extractor[A, B] })#E] {
      def contramap[A, C](eab: Extractor[A, B])(f: C => A): Extractor[C, B] = eab.contramap(f)
    }
}
