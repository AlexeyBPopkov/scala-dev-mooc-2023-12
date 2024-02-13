package module2

import scala.language.implicitConversions

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _)) }

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _)) }

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _)) }


  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit ffa: F[A] => Bindable[F, A], ffb: F[B] => Bindable[F, B]): F[(A, B)] = {
    Bindable.apply(fa).flatMap(a => Bindable.apply(fb).map((a, _)))
  }

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map((a, _)))

  object Bindable {
    def apply[F[_], A](fa: F[A])(implicit ffa: F[A] => Bindable[F, A]): Bindable[F, A] = ffa(fa)

    implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] =
      new Bindable[Option, A] {
        def map[B](f: A => B): Option[B] = opt.map(f)
        def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
      }

    implicit def listBindable[A](opt: List[A]): Bindable[List, A] =
      new Bindable[List, A] {
        def map[B](f: A => B): List[B] = opt.map(f)
        def flatMap[B](f: A => List[B]): List[B] = opt.flatMap(f)
      }

    implicit def eitherBindable[E, A](either: Either[E, A]): Bindable[({type lambda[X] = Either[E, X]})#lambda, A] =
      new Bindable[({type lambda[X] = Either[E, X]})#lambda, A] {
        def map[B](f: A => B): Either[E, B] = either.map(f)
        def flatMap[B](f: A => Either[E, B]): Either[E, B] = either.flatMap(f)
      }
  }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(20)

  lazy val t3: Unit = println(tupleBindable(optA, optB))
  lazy val t4: Unit = println(tupleBindable(list1, list2))

  lazy val r1: Unit = println(tuplef(optA, optB))
  lazy val r2: Unit = println(tuplef(list1, list2))
  lazy val r3: Unit = println(tuplef(either1, either2))

}