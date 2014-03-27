package stacycurl.scala.extractors

import scalaz._
import scalaz.std.list._
import scalaz.syntax.std.boolean._


object Extractor {
  def from[A] = new FromCapturer[A]
  def map[A]  = new MapCapturer[A]
  def fromMap[K, V](entries: (K, V)*): Extractor[K, V] = FromMap[K, V](entries.toMap)
  def fromMap[K, V](map: Map[K, V]): Extractor[K, V] = FromMap[K, V](map)
  def when[A](p: A => Boolean): Extractor[A, A] = When[A](p)
  def unzip[A, B, F[_]](implicit U: scalaz.Unzip[F]): Extractor[F[(A, B)], (F[A], F[B])] = Unzip[A, B, F](U)
  def orElse[A, B](alternatives: Extractor[A, B]*): Extractor[A, B] = first[A, B](alternatives: _*)
  def first[A, B](alternatives: Extractor[A, B]*): Extractor[A, B] = First[A, B](alternatives.toList)
  def last[A, B](alternatives: Extractor[A, B]*): Extractor[A, B] = Last[A, B](alternatives.toList)
  def never[A, B]: Extractor[A, B] = Never.asInstanceOf[Extractor[A, B]]

  object string {
    def contains(sub: String): Extractor[String, String] = Contains(sub)
    def regex[A](regex: String): RegexCapturer[A] = new RegexCapturer[A](regex)

    class RegexCapturer[A](regex: String) {
      def apply(pf: PartialFunction[List[String], A]): Extractor[String, A] = Extractor.Regex[A](regex, pf)
    }

    private case class Contains(sub: String) extends Extractor[String, String] {
      def unapply(s: String): Option[String] = s.contains(sub).option(s)
      override def describe: String = s"Contains($sub)"
    }
  }

  class FromCapturer[A] {
    def apply[B](f: A => Option[B]): Extractor[A, B] = OptFunction[A, B](f)
    def pf[B](pf: PartialFunction[A, B]): Extractor[A, B] = Partial[A, B](pf)
  }

  class MapCapturer[A] {
    def apply[B](f: A => B): Extractor[A, B] = Function[A, B](f)
  }

  object monoid {
    object first   { implicit def extractorMonoid[A, B] = apply[A, B](_ orElse _)                           }
    object last    { implicit def extractorMonoid[A, B] = apply[A, B](_ last _)                             }
    object append  { implicit def extractorMonoid[A, B](implicit S: Semigroup[B]) = apply[A, B](_ append _) }

    def apply[A, B](f: (Extractor[A, B], => Extractor[A, B]) => Extractor[A, B]): Monoid[Extractor[A, B]] =
      Monoid.instance[Extractor[A, B]](f, never[A, B])
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
    def arr[A, B](f: A => B): Extractor[A, B] = Function[A, B](f)
    def first[A, B, C](eab: Extractor[A, B]): Extractor[(A, C), (B, C)] = ArrFirst[A, B, C](eab)
    def compose[A, B, C](ebc: Extractor[B, C], eab: Extractor[A, B]): Extractor[A, C] = ebc.compose(eab)
    override def mapfst[A, B, C](eab: Extractor[A, B])(f: C => A): Extractor[C, B] = eab.contramap(f)
    override def mapsnd[A, B, C](eab: Extractor[A, B])(f: B => C): Extractor[A, C] = eab.map(f)
  }

  implicit def extractorUnzip[A]: scalaz.Unzip[({ type E[B] = Extractor[A, B] })#E] =
    new scalaz.Unzip[({ type E[B] = Extractor[A, B] })#E] {
      def unzip[B, C](ebc: Extractor[A, (B, C)]): (Extractor[A, B], Extractor[A, C]) = ebc.unzip
    }

  private case class OptFunction[A, B](f: A => Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
  }

  private case class Function[A, B](f: A => B) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = Some(f(a))
    override def describe = "Function"
  }

  // These classes are unnecessary as Extractor.{map, contramap, compose, andThen, orElse} could
  // Construct 'Function' with an appropriate function, I prefer to keep them for now to retain
  // the structure of the extractor, it's not different from an ordinary function but I plan to
  // add lables & toStrings to extractors
  private case class Partial[A, B](override val pf: PartialFunction[A, B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = pf.lift(a)
    override def describe: String = "Partial"
  }

  private case class FromMap[A, B](map: Map[A, B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = map.get(a)
    override def describe: String = s"FromMap(size = ${map.size})"
  }

  private case class When[A](p: A => Boolean) extends Extractor[A, A] {
    def unapply(a: A): Option[A] = p(a).option(a)
    override def describe: String = "When"
  }

  private case class Mapped[A, B, C](ab: Extractor[A, B], bc: B => C) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab(a).map(bc)
    override def describe: String = ab.describe + ".map"
  }

  private case class FlatMapped[A, B, C](ab: Extractor[A, B], bac: B => Extractor[A, C]) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab(a).flatMap(bac(_)(a))
  }

  private case class Contramapped[A, B, C](ab: Extractor[A, B], ca: C => A) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ab(ca(c))
    override def describe: String = ab.describe + ".contramap"
  }

  private case class Compose[A, B, C](ab: Extractor[A, B], ca: Extractor[C, A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ca(c).flatMap(ab)
    override def describe: String = s"Compose(${ab.describe}, ${ca.describe})"
  }

  private case class AndThen[A, B, C](ab: Extractor[A, B], ca: Extractor[C, A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ca(c).flatMap(ab)
    override def describe: String = s"AndThen(${ca.describe}, ${ab.describe})"
  }

  // The first element to be added, to the _head_ of the list, requires appending to the end
  private case class First[A, B](alternatives: List[Extractor[A, B]]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = alternatives.toStream.flatMap(alternative => alternative(a)).headOption
    override def describe: String = "First(%s)".format(alternatives.map(_.describe).mkString(", "))

    override def first(alternative: Extractor[A, B]): Extractor[A, B] = copy(alternatives :+ alternative)
  }

  // The last element to be added, to the _head_ of the list, i.e. stored as the first, bit odd.
  private case class Last[A, B](alternatives: List[Extractor[A, B]]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = alternatives.toStream.flatMap(alternative => alternative(a)).headOption
    override def describe: String = "Last(%s)".format(alternatives.reverse.map(_.describe).mkString(", "))

    override def last(alternative: Extractor[A, B]): Extractor[A, B] = copy(alternative :: alternatives)
  }

  private case class Append[A, B](lhs: Extractor[A, B], rhs: Extractor[A, B], S: Semigroup[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = scalaz.std.option.optionMonoid[B](S).append(lhs(a), rhs(a))
    override def describe: String = s"Append(${lhs.describe}, ${rhs.describe})"
  }

  private case class OrThrow[A, B](ab: Extractor[A, B], exception: A => Exception) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).orElse(throw exception(a))
    override def describe: String = s"OrThrow(${ab.describe})"
  }

  private case class GetOrElse[A, B](ab: Extractor[A, B], alternative: B) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).orElse(Some(alternative))
  }

  private case class Filter[A, B](ab: Extractor[A, B], p: B => Boolean) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).filter(p)
    override def describe: String = s"Filter(${ab.describe})"
  }

  private case class Point[A, B](b: Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = b
  }

  private class Id[A] extends Extractor[A, A] {
    def unapply(a: A): Option[A] = Some(a)
  }

  private object Never extends Extractor[Nothing, Nothing] {
    def unapply(n: Nothing): Option[Nothing] = None
    override def describe: String = "Never"
  }

  private case class ArrFirst[A, B, C](ab: Extractor[A, B]) extends Extractor[(A, C), (B, C)] {
    def unapply(ac: (A, C)): Option[(B, C)] = ab(ac._1).map(b => (b, ac._2))
  }

  private case class Unzip[A, B, F[_]](unzip: scalaz.Unzip[F]) extends Extractor[F[(A, B)], (F[A], F[B])] {
    def unapply(fab: F[(A, B)]): Option[(F[A], F[B])] = Some(unzip.unzip(fab))
    override def describe: String = "Unzip"
  }

  private case class Zip[A, B, C, D](ab: Extractor[A, B], cd: Extractor[C, D]) extends Extractor[(A, C), (B, D)] {
    def unapply(ac: (A, C)): Option[(B, D)] = for { b <- ab(ac._1); d <- cd(ac._2) } yield (b, d)
    override def describe: String = s"Zip(${ab.describe}, ${cd.describe})"
  }

  private case class Lens[A, B, C](ab: Extractor[A, B], lens: scalaz.Lens[B, C]) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab(a).map(lens.get)
    override def describe: String = s"Lens(${ab.describe})"
  }

  private case class Regex[A](regex: String, rpf: PartialFunction[List[String], A]) extends Extractor[String, A] {
    def unapply(s: String): Option[A] = regex.r.unapplySeq(s).flatMap(rpf.lift)
    override def describe: String = s"Regex($regex)"
  }

  private case class LiftToOption[A, B](ab: Extractor[A, B]) extends Extractor[Option[A], B] {
    def unapply(oa: Option[A]): Option[B] = oa.flatMap(ab)
    override def describe: String = s"LiftToOption(${ab.describe})"
  }

  private case class ForAll[A, B, F[_]: MonadPlus: Foldable](ab: Extractor[A, B]) extends Extractor[F[A], F[B]] {
    private val M = implicitly[MonadPlus[F]]
    private val F = implicitly[Foldable[F]]

    def unapply(fa: F[A]): Option[F[B]] = {
      val result = M.bind(fa)(a => optionToF(ab(a)))

      (F.length(result) == F.length(fa)).option(result)
    }

    private def optionToF[C](oc: Option[C]): F[C] = oc.fold(M.empty[C])(c => M.point[C](c))
  }

  private case class Exists[A, B, F[_]: MonadPlus: Foldable](ab: Extractor[A, B]) extends Extractor[F[A], F[B]] {
    private val M = implicitly[MonadPlus[F]]
    private val F = implicitly[Foldable[F]]

    def unapply(fa: F[A]): Option[F[B]] = {
      val result = M.bind(fa)(a => optionToF(ab(a)))

      (!F.empty(result) || F.empty(fa)).option(result)
    }

    private def optionToF[C](oc: Option[C]): F[C] = oc.fold(M.empty[C])(c => M.point[C](c))
  }
}

trait Extractor[A, B] extends (A => Option[B]) {
  def apply(a: A): Option[B] = unapply(a)
  def unapply(a: A): Option[B]
  def describe: String = ""

  def fn: (A => Option[B]) = this

  val pf: PartialFunction[A, B] = new PartialFunction[A, B] {
    def isDefinedAt(a: A): Boolean = unapply(a).isDefined
    def apply(a: A): B = unapply(a).get
  }

  def map[C](f: B => C): Extractor[A, C] = Extractor.Mapped[A, B, C](this, f)
  def flatMap[C](f: B => Extractor[A, C]): Extractor[A, C] = Extractor.FlatMapped[A, B, C](this, f)
  def contramap[C](f: C => A): Extractor[C, B] = Extractor.Contramapped[A, B, C](this, f)

  def compose[C](eca: Extractor[C, A]): Extractor[C, B] = Extractor.Compose[A, B, C](this, eca)
  def andThen[C](ebc: Extractor[B, C]): Extractor[A, C] = Extractor.AndThen[B, C, A](ebc, this)

  def orElse(alternative: Extractor[A, B]): Extractor[A, B] = first(alternative)
  def first(alternative: Extractor[A, B]): Extractor[A, B] = Extractor.First[A, B](List(this, alternative))
  def last(alternative: Extractor[A, B]): Extractor[A, B] = Extractor.Last[A, B](List(alternative, this))
  def orThrow(exception: Exception): Extractor[A, B] = Extractor.OrThrow[A, B](this, _ => exception)
  def orThrow(f: A => Exception): Extractor[A, B] = Extractor.OrThrow[A, B](this, f)
  def getOrElse(alternative: B): Extractor[A, B] = Extractor.GetOrElse[A, B](this, alternative)

  def append(alternative: Extractor[A, B])(implicit S: Semigroup[B]): Extractor[A, B] =
    Extractor.Append[A, B](this, alternative, S)

  def filter(p: B => Boolean): Extractor[A, B] = Extractor.Filter[A, B](this, p)
  def unzip[C, D](implicit ev: B =:= (C, D)): (Extractor[A, C], Extractor[A, D]) = (map(_._1), map(_._2))
  def zip[C, D](f: Extractor[C, D]): Extractor[(A, C), (B, D)] = Extractor.Zip[A, B, C, D](this, f)
  def lens[C](lens: Lens[B, C]): Extractor[A, C] = Extractor.Lens[A, B, C](this, lens)
  def liftToOption: Extractor[Option[A], B] = Extractor.LiftToOption[A, B](this)
  def forall[F[_]: MonadPlus: Foldable]: Extractor[F[A], F[B]] = Extractor.ForAll[A, B, F](this)
  def exists[F[_]: MonadPlus: Foldable]: Extractor[F[A], F[B]] = Extractor.Exists[A, B, F](this)
}
