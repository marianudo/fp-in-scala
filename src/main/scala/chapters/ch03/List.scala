package chapters.ch03

import scala.annotation.tailrec

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

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def prod3(ns: List[Int]): Int = foldLeft(ns, 1)(_ * _)
  def length2[A](as:List[A]): Int = foldLeft(as, 0)((a, _) => a + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def aux(f1: (B, A) => B): (A, B) => B = (a, b) => f1(b, a)

    foldRight(as, z)(aux(f))
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    def aux(f1: (A, B) => B): (B, A) => B = (b, a) => f1(a, b)

    foldLeft(as, z)(aux(f))
  }

  def appendInTermsOfFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  def appendInTermsOfFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l1, l2)((b, a) => Cons(a, b))

  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(appendInTermsOfFoldRight)

  def add1(li: List[Int]): List[Int] =
    foldRight(li, List[Int]())((i, acc) => Cons(i + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)

  def flatMapMine[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else List())

  def addElements(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addElements(t1, t2))
    case (Nil, Cons(h2, t2)) => Cons(h2, t2)
    case (Cons(h1, t1), Nil) => Cons(h1, t1)
    case (Nil, Nil) => Nil
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}
