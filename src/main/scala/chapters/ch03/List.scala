package chapters.ch03

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Tail of a Nil List")
    case Cons(_, t) => t
  }

  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead on Nil list")
    case Cons(_, t) => Cons(a, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def tail2[A](l: List[A]): List[A] = drop(l, 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def curriedDropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => curriedDropWhile(t)(f)
    case _ => as }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Int]): Int = foldRight(ns, 1)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, a) => a + 1)

}
