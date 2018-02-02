package chapters.ch02

import scala.annotation.tailrec

object HOF {
  def fib(n: Int): List[Int] = {
    @tailrec
    def aux(acc: List[Int], _n: Int): List[Int] = {
      def first(l: List[Int]): Int = l.head
      def second(l: List[Int]): Int = l.tail.head

      if(_n == 1) acc
      else aux((first(acc) + second(acc)) :: acc, _n - 1)
    }

    aux(1 :: 0 :: Nil, n).reverse
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def aux(acc: Boolean, _as: Array[A]): Boolean = {
      if(_as.length < 2) true
      else {
        val current = acc && ordered(_as(0), _as(1))

        if(_as.length == 2) current
        else aux(acc && current, _as.slice(2, _as.length - 1))
      }
    }

    aux(true, as)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

}
