package chapters.ch05

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def aux(acc: List[A], as: Stream[A]): List[A] = as match {
      case Empty => acc
      case Cons(h, t) => aux(h() :: acc, t())
    }

    aux(List.empty, this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))

  implicit class Sum(s: Stream[Int]) {
    def sum: Int = s match {
      case Empty => 0
      case Cons(h, t) => h() + t().sum
    }
  }
}
