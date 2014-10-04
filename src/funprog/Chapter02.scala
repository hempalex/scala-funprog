package funprog

object Chapter02 {

   // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if (n > 0) go(b, a+b, n-1) else a
    }
    go(0, 1, n)
  }
  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length)
        true
      else
      if (ordered(as(n-1), as(n)))
        loop(n+1)
      else
        false
    }

    loop(1)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
