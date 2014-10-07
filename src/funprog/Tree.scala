package funprog

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1
    case _ => 1
  }
}