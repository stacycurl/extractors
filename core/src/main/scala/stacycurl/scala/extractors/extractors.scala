package stacycurl.scala.extractors

import scalaz._
import scalaz.syntax.std.boolean._


object Extractor extends ExtractorInstances {
  def from[A] = new FromCapturer[A]
  def map[A]  = new MapCapturer[A]
  def when[A](f: A => Boolean): Extractor[A, A] = Function[A, A]((a: A) => f(a).option(a))

  object string {
    def contains(sub: String): Extractor[String, String] = when[String](_.contains(sub))
  }

  class FromCapturer[A] {
    def apply[B](f: A => Option[B]): Extractor[A, B] = Function[A, B](f)
    def pf[B](pf: PartialFunction[A, B]): Extractor[A, B] = apply(pf.lift)
  }

  class MapCapturer[A] {
    def apply[B](f: A => B): Extractor[A, B] = Function[A, B]((a: A) => Option(f(a)))
  }

  private case class Function[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }

  private case class Mapped[A, B, C](from: Extractor[A, B], f: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = from.unapply(a).map(f)
  }

  private case class Contravariant[A, B, C](from: Extractor[A, B], f: C => A) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = from.unapply(f(c))
  }

  private case class Compose[A, B, C](eab: Extractor[A, B], eca: Extractor[C, A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = eca.unapply(c).flatMap(a => eab.unapply(a))
  }
}

trait Extractor[A, B] {
  def unapply(a: A): Option[B]

  def map[C](f: B => C): Extractor[A, C] = Extractor.Mapped[A, B, C](this, f)
  def contramap[C](f: C => A): Extractor[C, B] = Extractor.Contravariant[A, B, C](this, f)
  def compose[C](eca: Extractor[C, A]): Extractor[C, B] = Extractor.Compose[A, B, C](this, eca)
  def andThen[C](ebc: Extractor[B, C]): Extractor[A, C] = ebc.compose(this)
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
