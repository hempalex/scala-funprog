import org.scalatest._
import funprog.List
import funprog.List._

class Chapter03 extends FlatSpec with Matchers {
  val list = List(1, 2, 3)

  "tail" should "get tail" in {
    tail(list) should be (List(2,3))
  }

  "drop" should "drop" in {
    drop(list, 2) should be (List(3))
  }

  it should "not drop(0)" in {
    drop(list, 0) should be (list)
  }

  "setHead" should "sethead" in {
     setHead(list, 4) should be (List(4,2,3))
  }

  "dropWhile" should "drop while predicate" in {
    dropWhile(list)(_ < 3) should be (List(3))
  }

  "append" should "append" in {
    append(list, List(4,5)) should be (List(1,2,3,4,5))
  }

  "init" should "all but not last" in {
    init(list) should be (List(1,2))
  }

  // 3.11
  "foldRight" should "able to sum" in {
    foldRight(list, 0)(_ + _) should be (6)
  }

  "foldRightViaReverse" should "able to sum" in {
    foldRightViaReverse(list, 0)(_ + _) should be (6)
  }
  // 3.11
  it should "able to product" in {
    foldRight(list, 1)(_ * _) should be (6)
  }
  // 3.11
  "foldLeft" should "able to sum" in {
    foldLeft(list, 0)(_ + _) should be (6)
  }
  // 3.11
  it should "able to product" in {
    foldLeft(list, 1)(_ * _) should be (6)
  }
  // 3.11
  "len" should "compute length" in {
    len(list) should be (3)
  }

  "reverse" should "reverse list" in {
    reverse(list) should be (List(3,2,1))
  }

  "foldLeftViaFoldRight" should "able to sum" in {
    foldLeftViaFoldRight(list, 0)(_ + _) should be (6)
  }

  "foldRightViaFoldLeft" should "able to sum" in {
    foldRightViaFoldLeft(list, 0)(_ + _) should be (6)
  }


  "appendViaFoldLeft" should "append" in {
    appendViaFoldLeft(list, List(4,5)) should be (List(1,2,3,4,5))
  }

  "appendViaFoldRight" should "append" in {
    appendViaFoldRight(list, List(4,5)) should be (List(1,2,3,4,5))
  }

  "concat" should "concatenate lists" in {
    concat(List(list, List(4,5))) should be (List(1,2,3,4,5))
  }

  // 3.16
  "map" should "mapping values" in {
    map(list)(_ + 1) should be (List(2,3,4))
  }
  // 3.17
  it should "mapping values to strings" in {
    map(List(1.1, 2.2, 3.3))(_.toString) should be (List("1.1", "2.2", "3.3"))
  }

  "filter" should "filter values" in {
    filter(list)(_ > 2) should be (List(3))
  }

  "flatMap" should "flattern mapping" in {
    flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "filterViaFlatMap" should "filter values" in {
    filterViaFlatMap(list)(_ > 2) should be (List(3))
  }

   // 3.22
  "zipWith" should "zipping values with function" in {
    zipWith(List(1,2,3), List(4,5,6))(_ + _) should be (List(5,7,9))
  }
}
