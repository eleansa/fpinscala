package fpinscala.datastructures

import scala.annotation.tailrec

/**
 * Created by ele on 26/04/2014.
 */
object ListExample extends App {

  val elements = List(1,2,3)
  val elements2 = List(4,5,6)
  val elements3: List[Double] = List(5.0,6.0,7.0)
  val cities = List("Athens", "Buenos Aires", "London", "Hong Kong", "New York")
  val subElements = List(2,3)

  // StackOverflow when elements are 10000
  def buildListReverse(n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case _ => Cons(n, buildListReverse(n - 1))
    }
  }

  // Stack safe because it is tail recursive
  def buildList(n: Int): List[Int] = {
    @tailrec
    def go(x: Int, xs: List[Int]): List[Int] = {
      x match {
        case 0 => xs
        case _ => go(x - 1, Cons(x, xs))
      }
    }
    go(n, Nil)
  }

  val list: List[Int] = buildList(4)
  println(s"List:                               ${list}")
  println(s"List length:                        ${List.length(list)}")

  println(s"Numbers:                            ${List.length[Int](elements)}")
  println(s"Cities:                             ${List.length[String](cities)}")
  println(s"Cities length3:                     ${List.length3[String](cities)}")

  println(s"Cons foldRight:                     ${List.foldRight(elements, Nil: List[Int])(Cons(_,_))}")

  println(s"Append via foldLeft:                ${List.appendViaFoldLeft(elements, elements2)}")
  println(s"Append via foldRight:               ${List.appendViaFoldRight(elements, elements2)}")
  println(s"Add 1 to ${elements}:               ${List.add1(elements)}")
  println(s"Double to String of ${elements3}:   ${List.doubleToString(elements3)}")
  println(s"Map - add 3 to ${elements}:         ${List.map(elements) (a => a + 3)}")

  println(s"Remove odd numbers from ${elements}: ${List.filter(elements) (_ % 2 == 0)}")
  println(s"FlatMap on ${elements}:             ${List.flatMap(elements) (i => List(i,i))}")
  println(s"Remove odd numbers via FlatMap from ${elements}: ${List.filterViaFlatMap(elements) (_ % 2 == 0)}")

  println(s"Add Lists ${elements}, ${elements2}:${List.addLists(elements, elements2)}")

  val elems = List(1,2,3,4,5)

  println(s"HasSubsequent (1,2) in (1,2,3,4,5): ${List.hasSubsequence(elems, List(1,2))}")
  println(s"HasSubsequent (2,3) in (1,2,3,4,5): ${List.hasSubsequence(elems, List(2,3))}")
  println(s"HasSubsequent (4) in (1,2,3,4,5):   ${List.hasSubsequence(elems, List(4))}")


  println(s"HasSubsequent (false):              ${List.hasSubsequence(List(1,2,3), List(5))}")
  println(s"HasSubsequent (false):              ${List.hasSubsequence(List(1,2,3), List(3,4))}")
  println(s"HasSubsequent (true):               ${List.hasSubsequence(List(1,2,3), List(3))}")
}
