package fpinscala.laziness

import Stream._
import scala.annotation.tailrec

trait Stream[+A] {

  def toListRecursive: List[A] = {
    def go(l: Stream[A]): List[A] = {
      l match {
        case Empty => Nil
        case Cons(h, t) => h() :: go(t())
      }
    }
    go(this)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Stream.empty
    }
    else Stream()

  // Skip n first elements
  def drop(n: Int): Stream[A] = {
    @tailrec
    def go[A](s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
      else s match {
        case Empty => Empty
        case Cons(_,t) => go(t(), n-1)
      }
    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def go(as: Stream[A]): Stream[A] = {
      as match {
        case Cons(h, t) if (p(h())) =>  cons(h(), go(t()))
        case _ => Stream.empty
      }
    }
    go(this)
  }

  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,acc) => if (p(h)) cons(h, acc) else Empty)

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => if (p(h())) t().forAll(p) else false
      case Empty => true
    }


  def headOption: Option[A] =
    this match {
      case Cons(h, _) => Some(h())
      case _ => None
    }

  def headOption1: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,acc) => cons(f(h),acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, acc) => if (p(h)) cons(h, acc) else Empty)

  def append[B>:A](bs: Stream[B]): Stream[B] =
    foldRight(bs)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, acc) => f(h).append(acc))


  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)){
      case (Cons(h, t), i) if (i > 0) => Some(h(), (t(), i - 1))
      case _ => None
    }
  }

  def takeWhileViaUnFold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, b)){
      case (Cons(a, as), Cons(b, bs)) =>
        val h = f(a(), b())
        Some(h, (as(), bs()))
      case _ => None
    }

  def zipAll[B,C](b: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, b)){
      case (Cons(a, as), Cons(b, bs)) => {
        val h = (Some(a()), Some(b()))
        Some(h, (as(), bs()))
      }
      case (Cons(a, as), Empty) => {
        val h = (Some(a()), None)
        Some(h, (as(), Empty))
      }
      case (Empty, Cons(b, bs)) => {
        val h = (None, Some(b()))
        Some(h, (Empty, bs()))
      }
      case _ => None
    }

  def startsWith[B>:A](s: Stream[B]): Boolean =
    this.zipWith(s)((a:A,b:B) => a == b).forAll(_ == true)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append (Stream(empty))

  // It doesn't work - it doesn't contain the empty stream at the end!!!! AAAA!!!
  def tails_WithoutEmptyStreamAtTheEnd: Stream[Stream[A]] =
    unfold(this){
      case Cons(a, as) =>
        val h: Stream[A] = cons(a(), as())
        Some(h, as())
      case Empty => None
    } append (Stream.empty)


  def hasSubsequence[B>:A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A):Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(prev: Int, curr: Int): Stream[Int] = {
      val next = prev + curr
      cons(prev, go(curr, next))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)){ case (prev, curr) => Some((prev, (curr, prev + curr))) }


  def fromViaUnfold(n: Int): Stream[Int] =
    unfold((n,n+1)){ case (prev, curr) => Some((prev, (curr, curr + 1) ))}

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)( x => Some(x, x ))

  def onesViaConstant: Stream[Int] =
    constantViaUnfold(1)


}