package fpinscala.datastructures

/**
 * Created by ele on 27/04/2014.
 */
object TreeExample extends App {


  val tree = Branch(Branch(Leaf(4), Branch(Leaf(7), Leaf(Nil))), Leaf(5))
  val intTree: Tree[Int] = Branch[Int](Branch[Int](Leaf(4), Branch[Int](Leaf(7), Leaf(2))), Leaf(5))

  println(s"Tree size:            ${Tree.size(tree)}")
  println(s"Tree size:            ${Tree.size(Branch(Leaf(Nil), Leaf(Nil)))}")

  println(s"Tree max el:          ${Tree.max1(intTree)}")
  println(s"Tree max2 el:         ${Tree.max2(intTree)}")

}
