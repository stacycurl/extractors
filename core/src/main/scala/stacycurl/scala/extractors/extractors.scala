package stacycurl.scala.extractors

import scala.reflect.ClassTag
import scalaz._
import scalaz.std.list._
import scalaz.syntax.std.boolean._


object Extractor {
  def apply[A, B](f: A => Option[B], name: String = null): Extractor[A, B] = Apply[A, B](f, Option(name))
  def id[A]: Extractor[A, A] = new Id[A]
  def point[A, B](b: Option[B]): Extractor[A, B] = Point[A, B](b)
  def from[A] = new FromCapturer[A]
  def map[A]  = new MapCapturer[A]
  def fromMap[K, V](entries: (K, V)*): Extractor[K, V] = FromMap[K, V](entries.toMap)
  def fromMap[K, V](map: Map[K, V]): Extractor[K, V] = FromMap[K, V](map)
  def when[A](p: A => Boolean, name: String = null): Extractor[A, A] = When[A](p, Option(name))
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
      def describe: String = s"Contains($sub)"
    }
  }

  class FromCapturer[A] {
    def apply[B](f: A => Option[B], name: String = null): Extractor[A, B] = Apply[A, B](f, Option(name))
    def pf[B](pf: PartialFunction[A, B], name: String = null): Extractor[A, B] = Partial[A, B](pf, Option(name))
  }

  class MapCapturer[A] {
    def apply[B](f: A => B, name: String = null): Extractor[A, B] = Function[A, B](f, Option(name))
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
    def arr[A, B](f: A => B): Extractor[A, B] = Function[A, B](f, None)
    def first[A, B, C](eab: Extractor[A, B]): Extractor[(A, C), (B, C)] = ArrFirst[A, B, C](eab)
    def compose[A, B, C](ebc: Extractor[B, C], eab: Extractor[A, B]): Extractor[A, C] = ebc.compose(eab)
    override def mapfst[A, B, C](eab: Extractor[A, B])(f: C => A): Extractor[C, B] = eab.contramap(f)
    override def mapsnd[A, B, C](eab: Extractor[A, B])(f: B => C): Extractor[A, C] = eab.map(f)
  }

  implicit def extractorUnzip[A]: scalaz.Unzip[({ type E[B] = Extractor[A, B] })#E] =
    new scalaz.Unzip[({ type E[B] = Extractor[A, B] })#E] {
      def unzip[B, C](ebc: Extractor[A, (B, C)]): (Extractor[A, B], Extractor[A, C]) = ebc.unzip
    }

  private case class Apply[A, B](f: A => Option[B], name: Option[String] = None) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = f(a)
    def describe: String = parenthesise("Apply", name)
  }

  private case class Function[A, B](f: A => B, name: Option[String]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = Some(f(a))
    def describe = parenthesise("Function", name)
  }

  private case class Named[A, B](ab: Extractor[A, B], name: String) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab.unapply(a)
    def describe = s"${ab.describe}($name)"
  }

  // These classes exist to help make debugging easier, I will introduce a 'fuse' method to collapse them
  // down to 'Apply'
  private case class Partial[A, B](override val pf: PartialFunction[A, B], name: Option[String])
    extends Extractor[A, B] {

    def unapply(a: A): Option[B] = pf.lift(a)
    def describe: String = parenthesise("Partial", name)
  }

  private case class FromMap[A, B](map: Map[A, B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = map.get(a)
    def describe: String = s"FromMap(size = ${map.size})"
  }

  private case class When[A](p: A => Boolean, name: Option[String]) extends Extractor[A, A] {
    def unapply(a: A): Option[A] = p(a).option(a)
    def describe: String = parenthesise("When", name)
  }

  private case class Mapped[A, B, C](ab: Extractor[A, B], bc: B => C, name: Option[String]) extends Extractor[A, C] {
    def unapply(a: A): Option[C] = ab(a).map(bc)
    def describe: String = parenthesise(ab.describe + ".map", name)
  }

  private case class FlatMapped[A, B, C](ab: Extractor[A, B], bac: B => Extractor[A, C], name: Option[String])
    extends Extractor[A, C] {

    def unapply(a: A): Option[C] = ab(a).flatMap(bac(_)(a))
    def describe: String = parenthesise(ab.describe + ".flatMap", name)
  }

  private case class Contramapped[A, B, C](ab: Extractor[A, B], ca: C => A, name: Option[String])
    extends Extractor[C, B] {

    def unapply(c: C): Option[B] = ab(ca(c))
    def describe: String = parenthesise(ab.describe + ".contramap", name)
  }

  private case class Compose[A, B, C](ab: Extractor[A, B], ca: Extractor[C, A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ca(c).flatMap(ab)
    def describe: String = s"${ab.describe}.compose(${ca.describe})"
  }

  private case class AndThen[A, B, C](ab: Extractor[A, B], ca: Extractor[C, A]) extends Extractor[C, B] {
    def unapply(c: C): Option[B] = ca(c).flatMap(ab)
    def describe: String = s"${ca.describe}.andThen(${ab.describe})"
  }

  // The first element to be added, to the _head_ of the list, requires appending to the end
  private case class First[A, B](alternatives: List[Extractor[A, B]]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = alternatives.toStream.flatMap(alternative => alternative(a)).headOption
    def describe: String = "First(%s)".format(alternatives.map(_.describe).mkString(", "))

    override def first(alternative: Extractor[A, B]): Extractor[A, B] = copy(alternatives :+ alternative)
  }

  // The last element to be added, to the _head_ of the list, i.e. stored as the first, bit odd.
  private case class Last[A, B](alternatives: List[Extractor[A, B]]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = alternatives.toStream.flatMap(alternative => alternative(a)).headOption
    def describe: String = "Last(%s)".format(alternatives.reverse.map(_.describe).mkString(", "))

    override def last(alternative: Extractor[A, B]): Extractor[A, B] = copy(alternative :: alternatives)
  }

  private case class Append[A, B](lhs: Extractor[A, B], rhs: Extractor[A, B], S: Semigroup[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = scalaz.std.option.optionMonoid[B](S).append(lhs(a), rhs(a))
    def describe: String = s"${lhs.describe}.append(${rhs.describe})"
  }

  private case class OrThrow[A, B](ab: Extractor[A, B], exception: A => Exception, name: Option[String])
    extends Extractor[A, B] {

    def unapply(a: A): Option[B] = ab(a).orElse(throw exception(a))
    def describe: String = parenthesise(s"${ab.describe}.orThrow", name)
  }

  private case class GetOrElse[A, B](ab: Extractor[A, B], alternative: B) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).orElse(Some(alternative))
    def describe: String = s"${ab.describe}.getOrElse($alternative)"
  }

  private case class Filter[A, B](ab: Extractor[A, B], p: B => Boolean, name: Option[String]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = ab(a).filter(p)
    def describe: String = parenthesise(s"${ab.describe}.filter", name)
  }

  private case class Point[A, B](b: Option[B]) extends Extractor[A, B] {
    def unapply(a: A): Option[B] = b
    def describe: String = s"Point($b)"
  }

  private class Id[A] extends Extractor[A, A] {
    def unapply(a: A): Option[A] = Some(a)
    def describe: String = "Id"
  }

  private object Never extends Extractor[Nothing, Nothing] {
    def unapply(n: Nothing): Option[Nothing] = None
    def describe: String = "Never"
  }

  private case class ArrFirst[A, B, C](ab: Extractor[A, B]) extends Extractor[(A, C), (B, C)] {
    def unapply(ac: (A, C)): Option[(B, C)] = ab(ac._1).map(b => (b, ac._2))
    def describe: String = s"ArrFirst(${ab.describe})"
  }

  private case class Unzip[A, B, F[_]](unzip: scalaz.Unzip[F]) extends Extractor[F[(A, B)], (F[A], F[B])] {
    def unapply(fab: F[(A, B)]): Option[(F[A], F[B])] = Some(unzip.unzip(fab))
    def describe: String = "Unzip"
  }

  private case class Zip[A, B, C, D](ab: Extractor[A, B], cd: Extractor[C, D]) extends Extractor[(A, C), (B, D)] {
    def unapply(ac: (A, C)): Option[(B, D)] = for { b <- ab(ac._1); d <- cd(ac._2) } yield (b, d)
    def describe: String = s"${ab.describe}.zip(${cd.describe})"
  }

  private case class Lens[A, B, C](ab: Extractor[A, B], lens: scalaz.Lens[B, C], name: Option[String])
    extends Extractor[A, C] {

    def unapply(a: A): Option[C] = ab(a).map(lens.get)
    def describe: String = parenthesise(s"${ab.describe}.lens", name)
  }

  private case class Regex[A](regex: String, rpf: PartialFunction[List[String], A]) extends Extractor[String, A] {
    def unapply(s: String): Option[A] = regex.r.unapplySeq(s).flatMap(rpf.lift)
    def describe: String = s"Regex($regex)"
  }

  private case class LiftToOption[A, B](ab: Extractor[A, B]) extends Extractor[Option[A], B] {
    def unapply(oa: Option[A]): Option[B] = oa.flatMap(ab)
    def describe: String = s"${ab.describe}.liftToOption"
  }

  private case class ForAll[A, B, F[_]](ab: Extractor[A, B])
    (implicit val M: MonadPlus[F], F: Foldable[F], C: ClassTag[F[_]]) extends Extractor[F[A], F[B]] {

    def unapply(fa: F[A]): Option[F[B]] = {
      val result = M.bind(fa)(a => optionToF(ab(a)))

      (F.length(result) == F.length(fa)).option(result)
    }

    def describe: String = "%s.forAll[%s]".format(ab.describe, C.runtimeClass.getSimpleName)

    private def optionToF[C](oc: Option[C]): F[C] = oc.fold(M.empty[C])(c => M.point[C](c))
  }

  private case class Exists[A, B, F[_]](ab: Extractor[A, B])
    (implicit val M: MonadPlus[F], F: Foldable[F], C: ClassTag[F[_]]) extends Extractor[F[A], F[B]] {

    def unapply(fa: F[A]): Option[F[B]] = {
      val result = M.bind(fa)(a => optionToF(ab(a)))

      (!F.empty(result) || F.empty(fa)).option(result)
    }

    def describe: String = "%s.exists[%s]".format(ab.describe, C.runtimeClass.getSimpleName)

    private def optionToF[C](oc: Option[C]): F[C] = oc.fold(M.empty[C])(c => M.point[C](c))
  }

  private def parenthesise(prefix: String, name: Option[String]): String =
    name.fold(prefix)(n => s"$prefix($n)")
}

trait Extractor[A, B] extends (A => Option[B]) {
  def apply(a: A): Option[B] = unapply(a)
  def unapply(a: A): Option[B]
  def describe: String
  def named(name: String): Extractor[A, B] = Extractor.Named[A, B](this, name)

  def fn: (A => Option[B]) = this

  val pf: PartialFunction[A, B] = new PartialFunction[A, B] {
    def isDefinedAt(a: A): Boolean = unapply(a).isDefined
    def apply(a: A): B = unapply(a).get
  }

  def map[C](f: B => C, name: String = null): Extractor[A, C] = Extractor.Mapped[A, B, C](this, f, Option(name))

  def flatMap[C](f: B => Extractor[A, C], name: String = null): Extractor[A, C] =
    Extractor.FlatMapped[A, B, C](this, f, Option(name))

  def contramap[C](f: C => A, name: String = null): Extractor[C, B] =
    Extractor.Contramapped[A, B, C](this, f, Option(name))

  def compose[C](eca: Extractor[C, A]): Extractor[C, B] = Extractor.Compose[A, B, C](this, eca)
  def andThen[C](ebc: Extractor[B, C]): Extractor[A, C] = Extractor.AndThen[B, C, A](ebc, this)

  def orElse(alternative: Extractor[A, B]): Extractor[A, B] = first(alternative)
  def first(alternative: Extractor[A, B]): Extractor[A, B] = Extractor.First[A, B](List(this, alternative))
  def last(alternative: Extractor[A, B]): Extractor[A, B] = Extractor.Last[A, B](List(alternative, this))

  def orThrow(exception: Exception): Extractor[A, B] =
    Extractor.OrThrow[A, B](this, _ => exception, Some(exception.toString))

  def orThrow(f: A => Exception, name: String = null): Extractor[A, B] = Extractor.OrThrow[A, B](this, f, Option(name))
  def getOrElse(alternative: B): Extractor[A, B] = Extractor.GetOrElse[A, B](this, alternative)

  def append(alternative: Extractor[A, B])(implicit S: Semigroup[B]): Extractor[A, B] =
    Extractor.Append[A, B](this, alternative, S)

  def filter(p: B => Boolean, name: String = null): Extractor[A, B] = Extractor.Filter[A, B](this, p, Option(name))
  def unzip[C, D](implicit ev: B =:= (C, D)): (Extractor[A, C], Extractor[A, D]) = (map(_._1), map(_._2))
  def zip[C, D](f: Extractor[C, D]): Extractor[(A, C), (B, D)] = Extractor.Zip[A, B, C, D](this, f)

  def lens[C](lens: Lens[B, C], name: String = null): Extractor[A, C] =
    Extractor.Lens[A, B, C](this, lens, Option(name))

  def liftToOption: Extractor[Option[A], B] = Extractor.LiftToOption[A, B](this)

  def forall[F[_]](implicit M: MonadPlus[F], F: Foldable[F], C: ClassTag[F[_]]): Extractor[F[A], F[B]] =
    Extractor.ForAll[A, B, F](this)

  def exists[F[_]](implicit M: MonadPlus[F], F: Foldable[F], C: ClassTag[F[_]]): Extractor[F[A], F[B]] =
    Extractor.Exists[A, B, F](this)

  def arrFirst[C]: Extractor[(A, C), (B, C)] = Extractor.extractorArrow.first[A, B, C](this)
}
