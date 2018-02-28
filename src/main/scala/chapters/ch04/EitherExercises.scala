package chapters.ch04

import scala.{Either => _}

object EitherExercises {

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(r) => Right(f(r))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }


    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(r) => Right(r)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)

    def foldRight[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Left(l) => Left(l)
      case Right(r) => b.map(bb => f(r, bb))
    }
  }

  object Either {
    def sequenceViaFold[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      es.foldRight[Either[E, List[A]]](Right(Nil))((ea, ela) => for {
        a <- ea
        la <- ela
      } yield a :: la
      )

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight[Either[E, List[B]]](Right(Nil))((a, elb) => for {
        b <- f(a)
        lb <- elb
      } yield b :: lb
      )

    def traverseViaPatternMatching[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case h :: t =>
        for {
          b <- f(h)
          lb <- traverseViaPatternMatching(t)(f)
        } yield b :: lb

      case Nil => Right(Nil)

        /*
        es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
         */
    }

    def traverseViaPatternMatchingAndMap2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverseViaPatternMatchingAndMap2(t)(f))(_ :: _)
    }

    def traverseViaMap2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight[Either[E, List[B]]](Right(Nil))((a, elb) => f(a).map2(elb)(_ :: _))

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(identity)
  }
}
