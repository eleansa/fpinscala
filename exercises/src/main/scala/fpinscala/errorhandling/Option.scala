package fpinscala.errorhandling


import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case Some(value) => value
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(value) => Some(value)
      case None => ob
    }
  }

  def orElse2[B>:A](ob: => Option[B]): Option[B] = {
    map (Some(_)) getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case opt @ Some(value) => if (f(value)) opt else None
    }
  }

  def filter2(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionExample extends App {

  val x = Some(3)
  val xNone: Option[Int] = None

  println(s"MAP: Some(3):      Expected Some(4) === ${x.map(_ + 1)}")
  println(s"MAP: None:         Expected None === ${xNone.map(_ + 1)}")

  println(s"GOE: Some(3):      Expected 3 === ${x.getOrElse("x")}")
  println(s"GOE: None:         Expected x === ${xNone.getOrElse("x")}")

  println(s"FLAT: Some(3):     Expected 3x === ${x.flatMap(a => Some(a + "x"))}")
  println(s"FLAT: None:        Expected None === ${xNone.flatMap(a => Some(a + "x"))}")

  println(s"OE: Some(3):       Expected 3 === ${x.orElse(Some("3x"))}")
  println(s"OE: None:          Expected 3x === ${xNone.orElse(Some("3x"))}")

  println(s"OE2: Some(3):       Expected 3 === ${x.orElse2(Some("3x"))}")
  println(s"OE2: None:          Expected 3x === ${xNone.orElse2(Some("3x"))}")

  println(s"FILTER: Some(3):   Expected 3 === ${x.filter(_ > 2)}")
  println(s"FILTER: None:      Expected None === ${xNone.filter(_ > 2)}")

  println(s"FILTER2: Some(3):  Expected 3 === ${x.filter2(_ > 2)}")
  println(s"FILTER2: None:     Expected None === ${xNone.filter2(_ > 2)}")
}