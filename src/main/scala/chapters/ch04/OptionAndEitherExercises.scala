package chapters.ch04

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

object OptionAndEitherExercises {
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
}
