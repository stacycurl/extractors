package stacycurl.scala.extractors

import scalaz._
import scalaz.syntax.std.boolean._


object Extractor {
  def from[A] = new FromCapturer[A]
  def fromMap[K, V](entries: (K, V)*): Extractor[K, V] = fromMap[K, V](entries.toMap)
  def fromMap[K, V](map: Map[K, V]): Extractor[K, V] = from[K](map.get)
  def map[A]  = new MapCapturer[A]
  def when[A](f: A => Boolean): Extractor[A, A] = from[A]((a: A) => f(a).option(a))
  def unzip[A, B, F[_]](implicit U: Unzip[F]): Extractor[F[(A, B)], (F[A], F[B])] = map[F[(A, B)]](U.unzip)

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

  implicit def extractorMonad[A]: Monad[({ type E[B] = Extractor[A, B] })#E] =
    new Monad[({ type E[B] = Extractor[A, B] })#E] {
      def point[B](b: => B): Extractor[A, B] = Extractor.Point[A, B](Some(b))
      override def map[B, C](eab: Extractor[A, B])(f: B => C): Extractor[A, C] = eab.map(f)
      def bind[B, C](eab: Extractor[A, B])(fbc: B => Extractor[A, C]): Extractor[A, C] = eab.flatMap(fbc)
    }

  implicit def extractorContravariant[B]: Contravariant[({ type E[A] = Extractor[A, B] })#E] =
    new Contravariant[({ type E[A] = Extractor[A, B] })#E] {
      def contramap[A, C](eab: Extractor[A, B])(f: C => A): Extractor[C, B] = eab.contramap(f)
    }

  implicit object extractorArrow extends Arrow[Extractor] {
    def id[A]: Extractor[A, A] = new Id[A]
    def arr[A, B](f: A => B): Extractor[A, B] = Function[A, B]((a: A) => Some(f(a)))
    def first[A, B, C](eab: Extractor[A, B]): Extractor[(A, C), (B, C)] = First[A, B, C](eab)
    def compose[A, B, C](ebc: Extractor[B, C], eab: Extractor[A, B]): Extractor[A, C] = ebc.compose(eab)
    override def mapfst[A, B, C](eab: Extractor[A, B])(f: C => A): Extractor[C, B] = eab.contramap(f)
    override def mapsnd[A, B, C](eab: Extractor[A, B])(f: B => C): Extractor[A, C] = eab.map(f)
  }

  private case class Function[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }

  // These classes are unnecessary as Extractor.{map, contramap, compose, andThen, orElse} could
  // Construct 'Function' with an appropriate function, I prefer to keep them for now to retain
  // the structure of the extractor, it's not different from an ordinary function but I plan to
  // add lables & toStrings to extractors
  private case class Mapped[A, B, C](ab: A => Option[B], bc: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab(a).map(bc)
  }

  private case class FlatMapped[A, B, C](ab: A => Option[B], bac: B => (A => Option[C])) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab(a).flatMap(bac(_)(a))
  }

  private case class Contramapped[A, B, C](ab: A => Option[B], ca: C => A) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ab(ca(c))
  }

  private case class Compose[A, B, C](ab: A => Option[B], ca: C => Option[A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ca(c).flatMap(ab)
  }

  private case class OrElse[A, B](alternatives: List[A => Option[B]]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = alternatives.toStream.flatMap(alternative => alternative(a)).headOption

    override def orElse(alternative: A => Option[B]): Extractor[A, B] = copy(alternatives :+ alternative)
  }

  private case class OrThrow[A, B](ab: A => Option[B], exception: A => Exception) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).orElse(throw exception(a))
  }

  private case class GetOrElse[A, B](ab: A => Option[B], alternative: B) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).orElse(Some(alternative))
  }

  private case class Filter[A, B](ab: A => Option[B], p: B => Boolean) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).filter(p)
  }

  private case class Point[A, B](b: Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = b
  }

  private class Id[A] extends Extractor[A, A] {
    def unapply(a: A): Option[A] = Some(a)
  }

  private case class First[A, B, C](ab: A => Option[B]) extends Extractor[(A, C), (B, C)] {
    def unapply(ac: (A, C)): Option[(B, C)] = ab(ac._1).map(b => (b, ac._2))
  }

  private case class Zip[A, B, C, D](ab: A => Option[B], cd: C => Option[D]) extends Extractor[(A, C), (B, D)] {
    def unapply(ac: (A, C)): Option[(B, D)] = for { b <- ab(ac._1); d <- cd(ac._2) } yield (b, d)
  }
}

trait Extractor[A, B] extends (A => Option[B]) {
  def apply(a: A): Option[B] = unapply(a)
  def unapply(a: A): Option[B]

  def fn: (A => Option[B]) = this
  def map[C](f: B => C): Extractor[A, C] = Extractor.Mapped[A, B, C](this, f)
  def flatMap[C](f: B => Extractor[A, C]): Extractor[A, C] = Extractor.FlatMapped[A, B, C](this, f)
  def contramap[C](f: C => A): Extractor[C, B] = Extractor.Contramapped[A, B, C](this, f)
  def compose[C](eca: C => Option[A]): Extractor[C, B] = Extractor.Compose[A, B, C](this, eca)
  def andThen[C](ebc: B => Option[C]): Extractor[A, C] = Extractor.Compose[B, C, A](ebc, this)
  def orElse(alternative: A => Option[B]): Extractor[A, B] = Extractor.OrElse[A, B](List(this, alternative))
  def orThrow(exception: Exception): Extractor[A, B] = Extractor.OrThrow[A, B](this, _ => exception)
  def orThrow(f: A => Exception): Extractor[A, B] = Extractor.OrThrow[A, B](this, f)
  def getOrElse(alternative: B): Extractor[A, B] = Extractor.GetOrElse[A, B](this, alternative)
  def filter(p: B => Boolean): Extractor[A, B] = Extractor.Filter[A, B](this, p)
  def zip[C, D](f: C => Option[D]): Extractor[(A, C), (B, D)] = Extractor.Zip[A, B, C, D](this, f)
}
