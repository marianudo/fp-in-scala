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
}
