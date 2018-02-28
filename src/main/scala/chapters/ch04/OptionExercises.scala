package chapters.ch04

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import cats.Monad

import scala.language.higherKinds
import scala.{Option => _}

object OptionExercises {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      flatMap(a => Some(f(a)))

    def map_2[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }


    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(a) => f(a)
      case _ => None
    }

    def flatMap_2[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(b) => b
      case None => default
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(ob)


    def filter(f: A => Boolean): Option[A] =
      if(map(f).getOrElse(false)) this
      else None

    def filter_1(f: A => Boolean): Option[A] =
      flatMap(a => if(f(a)) Some(a) else None)
  }

  case class Some[A](a: A) extends Option[A]
  case object None extends Option[Nothing]

  // Smart constructors
  def some[A](a: A): Option[A] = Some(a)
  def none: Option[Nothing] = None

  object Option {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

    def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((a, la) =>
        for {
          aa <- a
          laa <- la
        } yield aa :: laa
      )

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some(Nil))((a, b) => {
        for {
          bb <- f(a)
          bbb <- b
        } yield bb :: bbb
      })

    def traverseF[F[_], A, B](a: List[A])(f: A => F[B])(implicit ev: Monad[F]): F[List[B]] = {
      import cats.syntax.applicative._
      import cats.syntax.flatMap._
      import cats.syntax.functor._

      a.foldRight[F[List[B]]](List.empty[B].pure[F])((a, flb) => {
        for {
          aa <- f(a)
          lb <- flb
        } yield aa :: lb
      })
    }

    def traverseViaSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      sequence_2(a.map(f))

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(identity)
  }
}
