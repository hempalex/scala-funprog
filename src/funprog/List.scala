package funprog

import java.util.NoSuchElementException

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // 3.2
  def drop[A](xs: List[A], n: Int): List[A] =
     xs match {
        case Nil => Nil
        case Cons(_, t) => if (n > 0) drop(t, n - 1) else xs
      }

  // needed to zipWith
  def head[A](xs: List[A]): A = xs match {
    case Nil => throw new NoSuchElementException("head of empty list")
    case Cons(h, t) => h
  }

  // 3.3
  def tail[A](xs: List[A]): List[A] = drop(xs, 1)

  // 3.4
  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => Cons(newHead, t)
  }

  // 3.5
  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => xs
  }

  // needed to flatMap
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // 3.6
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) if t != Nil => Cons(h, init(t))
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](xs: List[A], zero: B)(f: (A, B) => B): B = xs match {
    case Nil => zero
    case Cons(h, t) => foldLeft(t, f(h, zero))(f)
  }

  // 3.9
  def len[A](xs: List[A]): Int = foldLeft(xs, 0)((x, y) => y + 1)

  // 3.12
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil:List[A])((h, t) => Cons(h, t))

  def foldRight[A, B](xs: List[A], zero: B)(f: (A, B) => B): B = xs match {
    case Nil => zero
    case Cons(h, t) => f(h, foldRight(t, zero)(f))
  }

  def foldRightViaReverse[A, B](xs: List[A], zero: B)(f: (A, B) => B): B = foldLeft(reverse(xs), zero)(f)

  // 3.13
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((a,g) => b => g(f(a,b)))(z)

  // 3.13
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  // 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, t) => Cons(h, t))

  // 3.14
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((h, t) => Cons(h, t))

  // 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((x:List[A], y:List[A]) => append(x, y))

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  // 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((x) => if (f(x)) List(x) else Nil)

  // 3.23
  def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = a1 match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h, head(a2)), zipWith(t, tail(a2))(f))
  }

}

