package funprog

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => l + r + 1)
  def maximum(t: Tree[Int]): Int = fold(t)(identity)((l, r) => l max r)
  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => (l max r) + 1)
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))

}
