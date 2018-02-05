package chapters.ch03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(t1, t2) => size(t1) + size(t2) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(t1, t2) => maximum(t1).max(maximum(t2))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(t1, t2) => 1 + depth(t1).max(depth(t2))
  }
}
