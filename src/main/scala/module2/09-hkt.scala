package module2

import scala.language.implicitConversions

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _)) }

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _)) }

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _)) }


  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit bindable: Bindable[F]): F[(A, B)] = {
    bindable.flatMap(fa, (a:A)=> bindable.map(fb, (b:B) => (a, b)))
  }

  trait Bindable[F[_]] {
    def map[A, B](x: F[A], f: A => B): F[B]
    def flatMap[A, B](x: F[A], f: A => F[B]): F[B]
  }

  object Bindable {
    implicit object optBindable extends Bindable[Option] {
      def map[A, B](opt: Option[A], f: A => B): Option[B] = opt.map(f)
      def flatMap[A, B](opt: Option[A], f: A => Option[B]): Option[B] = opt.flatMap(f)
    }
    implicit object listBindable extends Bindable[List] {
      def map[A, B](lst: List[A], f: A => B): List[B] = lst.map(f)
      def flatMap[A, B](lst: List[A], f: A => List[B]): List[B] = lst.flatMap(f)
    }
  }





  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  lazy val r1: Unit = println(tuplef(optA, optB))
  lazy val r2: Unit = println(tuplef(list1, list2))

}