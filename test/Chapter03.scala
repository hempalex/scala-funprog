import org.scalatest._
import funprog.{List, Tree, Branch, Leaf}

class Chapter03 extends FlatSpec with Matchers {
  val list = List(1, 2, 3)

  "List.tail" should "get tail" in {
    List.tail(list) should be (List(2,3))
  }

  "List.drop" should "drop" in {
    List.drop(list, 2) should be (List(3))
  }

  it should "not drop(0)" in {
    List.drop(list, 0) should be (list)
  }

  "List.setHead" should "sethead" in {
     List.setHead(list, 4) should be (List(4,2,3))
  }

  "List.dropWhile" should "drop while predicate" in {
    List.dropWhile(list)(_ < 3) should be (List(3))
  }

  "List.append" should "append" in {
    List.append(list, List(4,5)) should be (List(1,2,3,4,5))
  }

  "List.init" should "all but not last" in {
    List.init(list) should be (List(1,2))
  }

  // 3.11
  "List.foldRight" should "able to sum" in {
    List.foldRight(list, 0)(_ + _) should be (6)
  }

  "List.foldRightViaReverse" should "able to sum" in {
    List.foldRightViaReverse(list, 0)(_ + _) should be (6)
  }
  // 3.11
  it should "able to product" in {
    List.foldRight(list, 1)(_ * _) should be (6)
  }
  // 3.11
  "List.foldLeft" should "able to sum" in {
    List.foldLeft(list, 0)(_ + _) should be (6)
  }
  // 3.11
  it should "able to product" in {
    List.foldLeft(list, 1)(_ * _) should be (6)
  }
  // 3.11
  "List.len" should "compute length" in {
    List.len(list) should be (3)
  }

  "List.reverse" should "reverse list" in {
    List.reverse(list) should be (List(3,2,1))
  }

  "List.foldLeftViaFoldRight" should "able to sum" in {
    List.foldLeftViaFoldRight(list, 0)(_ + _) should be (6)
  }

  "List.foldRightViaFoldLeft" should "able to sum" in {
    List.foldRightViaFoldLeft(list, 0)(_ + _) should be (6)
  }

  "List.appendViaFoldLeft" should "append" in {
    List.appendViaFoldLeft(list, List(4,5)) should be (List(1,2,3,4,5))
  }

  "List.appendViaFoldRight" should "append" in {
    List.appendViaFoldRight(list, List(4,5)) should be (List(1,2,3,4,5))
  }

  "List.concat" should "concatenate lists" in {
    List.concat(List(list, List(4,5))) should be (List(1,2,3,4,5))
  }

  // 3.16
  "List.map" should "mapping values" in {
    List.map(list)(_ + 1) should be (List(2,3,4))
  }
  // 3.17
  it should "mapping values to strings" in {
    List.map(List(1.1, 2.2, 3.3))(_.toString) should be (List("1.1", "2.2", "3.3"))
  }

  "List.filter" should "filter values" in {
    List.filter(list)(_ > 2) should be (List(3))
  }

  "List.flatMap" should "flattern mapping" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "List.filterViaFlatMap" should "filter values" in {
    List.filterViaFlatMap(list)(_ > 2) should be (List(3))
  }

   // 3.22
  "List.zipWith" should "zipping values with function" in {
    List.zipWith(List(1,2,3), List(4,5,6))(_ + _) should be (List(5,7,9))
  }

  it should "working on diferent sizes of lists" in {
    List.zipWith(List(1,2), List(4,5,6))(_ + _) should be (List(5,7))
    List.zipWith(List(1,2,3), List(4,5))(_ + _) should be (List(5,7))
  }


  // 3.24
  "List.hasSubsequence" should "find subseqence" in {
    List.hasSubsequence(List(1,2,3,4), List(1,2)) should be (true)
    List.hasSubsequence(List(1,2,3,4), List(2,3)) should be (true)
    List.hasSubsequence(List(1,2,3,4), List(3,4)) should be (true)
    List.hasSubsequence(List(1,2,3,1,2,3,4,1), List(3,4)) should be (true)
  }

  it should "return false is not subsequence found" in {
    List.hasSubsequence(List(1,2,3,4), List(2,1)) should be (false)
    List.hasSubsequence(List(1,2,3,4), List(3,2)) should be (false)
    List.hasSubsequence(List(1,2,3,4), List(1,3)) should be (false)
  }

  val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  "Tree.size" should "return size of tree" in {
    Tree.size(t) should be (5)
  }

  "Tree.maximum" should "return max value in tree" in {
    Tree.maximum(t) should be (3)
  }

  "Tree.depth" should "return max depth in tree" in {
    Tree.depth(t) should be (3)
  }

  "Tree.map" should "map values in tree" in {
    Tree.map(t)(_ + 1) should be (Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  }


}
