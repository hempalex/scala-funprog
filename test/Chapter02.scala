import org.scalatest._
import funprog.Chapter02._

class Chapter02 extends FlatSpec with Matchers {

  "fib" should "generate fibbonachi numbers" in {
    (0 to 5).map(fib) should be (Vector(0, 1, 1, 2, 3, 5))
  }

  "isSorted" should "return true on sorted" in {
    isSorted(Array(1, 2, 3, 4, 5, 6, 7), (x: Int, y: Int) => x <= y) should be (true)
  }

  it should "return false on unsorted" in {
    isSorted(Array(3, 1, 2, 1, 1, 1, 8), (x: Int, y: Int) => x <= y) should be (false)
  }


  "curry" should "curry function" in {
    def sum(a: Int, b: Int) = a + b // generic sum function
    val csum = curry(sum)           // curried sum function
    val csum3 = csum(3)             // paritally applied sum function

    sum(1,2) should be (3)
    csum(1)(2) should be (3)
    csum3(5) should be (8)
  }

  "uncurry" should "uncurry function" in {
    def sum(a: Int, b: Int) = a + b // generic sum function

    val csum = curry(sum)           // curried sum function
    val csum3 = csum(3)             // paritally applied sum function

    sum(1,2) should be (3)
    csum(1)(2) should be (3)
    csum3(5) should be (8)
  }

  "compose" should "compose functions" in {
    def f(x: Double) = math.Pi / 2 - x
    val cos = compose(math.sin, f)

    cos(0) should be (math.cos(0))
    cos(math.Pi) should be (math.cos(math.Pi))
  }


}
