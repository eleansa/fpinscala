package fpinscala.datastructures

import scala.annotation.tailrec


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def max1(tree: Tree[Int]): Int = {
    def go(t: Tree[Int], maxEl: Int): Int = {
      t match {
        case Leaf(value) => value.max(maxEl)
        case Branch(left, right) => go(left, maxEl).max(go(right, maxEl))
      }
    }
    go(tree, Int.MinValue)
  }

  def max2(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(value) => value
        case Branch(left, right) => max2(left).max(max2(right))
      }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

}