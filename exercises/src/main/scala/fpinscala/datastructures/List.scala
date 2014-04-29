package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_,t) => Cons(h,t)
  }

  // assumption n >= l.length
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def initReverse[A](l: List[A]): List[A] = {
    def recursive(xs: List[A], l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(h, Nil) => xs
      case Cons(h, t) => recursive(Cons(h,xs), t)
    }
    recursive(Nil, l)
  }

  def init2[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init2(t))
    }

  def init3[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def length1[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(x, xs) => 1 + length(xs)
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,acc) => acc + 1)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
  }

  def sum3(xs: List[Int]): Int = {
    foldLeft(xs, 0)((a, b) => a + b)
  }

  def product3(xs: List[Int]): Int = {
    foldLeft(xs, 1)((a, b) => a * b)
  }

  def length3[A](xs: List[A]): Int = {
    foldLeft(xs, 0) ((acc, _) => acc + 1)
  }

  def reverse[A](xs: List[A]): List[A] = {
    foldLeft(xs, List[A]()) ((acc: List[A],h: A) => Cons(h,acc))
  }

  // Stack-safe
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(l), z)((b,a) => f(a,b))
  }

  def appendViaFoldLeft[A](as: List[A], bs: List[A]): List[A] = {
    foldLeft(reverse(as), bs) ((acc, h) => Cons(h, acc))
  }

  // not stack-safe
  def appendViaFoldRight[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs) ((h, acc) => Cons(h, acc))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append)
  }

  def add1(xs: List[Int]): List[Int] = {
    foldLeft(reverse(xs), List[Int]()) ((acc, h) => Cons(h + 1, acc))
  }

  def doubleToString(xs: List[Double]): List[String] =
    foldLeft(reverse(xs), List[String]()) ((acc, h) => Cons(h.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldLeft(reverse(l), List[B]()) ((acc, h) => Cons(f(h), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldLeft(reverse(l), List[A]()) ((acc, h) => if (f(h)) Cons(h, acc) else acc)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    foldLeft(l, List[B]()) ((acc, h) => appendViaFoldLeft(acc, f(h)))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((x) => if (f(x)) Cons(x, Nil) else Nil)
  }

  def addLists(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Nil) => Cons(x, xs)
      case (Nil, Cons(y, ys)) => Cons(y, ys)
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))

    }
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    @tailrec
    def go(a: List[A], b: List[A], contains: Boolean): Boolean = {
      (a, b) match {
        case (Nil, Nil) => contains
        case (Nil, _) => false
        case (_, Nil) => contains
        case (Cons(x, xs), Cons(y, ys)) => {
          if (x == y) go(xs, ys, true)
          else go (xs, b, false)
        }
      }
    }

    go(l, sub, false)
  }

}