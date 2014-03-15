package stacycurl.scala.extractors

import scalaz._
import scalaz.syntax.std.boolean._


object Extractor extends ExtractorInstances {
  def from[A] = new FromCapturer[A]
  def fromMap[K, V](map: Map[K, V]): Extractor[K, V] = from[K](map.get)
  def map[A]  = new MapCapturer[A]
  def when[A](f: A => Boolean): Extractor[A, A] = from[A]((a: A) => f(a).option(a))

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

  // These classes are unnecessary as Extractor.{map, contramap, compose, andThen, orElse} could
  // Construct 'Function' with an appropriate function, I prefer to keep them for now to retain
  // the structure of the extractor, it's not different from an ordinary function but I plan to
  // add lables & toStrings to extractors
  private case class Mapped[A, B, C](from: A => Option[B], f: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = from(a).map(f)
  }

  private case class Contravariant[A, B, C](from: A => Option[B], f: C => A) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = from(f(c))
  }

  private case class Compose[A, B, C](eab: A => Option[B], eca: C => Option[A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = eca(c).flatMap(a => eab(a))
  }

  private case class OrElse[A, B](alternatives: List[A => Option[B]]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = alternatives.toStream.flatMap(alternative => alternative(a)).headOption

    override def orElse(alternative: A => Option[B]): Extractor[A, B] = copy(alternatives :+ alternative)
  }
}

trait Extractor[A, B] extends (A => Option[B]) {
  def apply(a: A): Option[B] = unapply(a)
  def unapply(a: A): Option[B]

  def fn: (A => Option[B]) = this
  def map[C](f: B => C): Extractor[A, C] = Extractor.Mapped[A, B, C](this, f)
  def contramap[C](f: C => A): Extractor[C, B] = Extractor.Contravariant[A, B, C](this, f)
  def compose[C](eca: C => Option[A]): Extractor[C, B] = Extractor.Compose[A, B, C](this, eca)
  def andThen[C](ebc: B => Option[C]): Extractor[A, C] = Extractor.Compose[B, C, A](ebc, this)
  def orElse(alternative: A => Option[B]): Extractor[A, B] = Extractor.OrElse[A, B](List(this, alternative))
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
