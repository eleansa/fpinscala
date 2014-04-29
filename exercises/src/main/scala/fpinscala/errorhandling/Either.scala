package fpinscala.errorhandling


import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = {
   this match {
     case Right(a) => Right(f(a))
     case Left(e) => Left(e)
   }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
     case Right(a) => f(a)
     case Left(e) => Left(e)
   }
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   this match {
     case Right(a) => Right(a)
     case Left(_) => b
   }
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   (this, b) match {
     case (_, Left(e)) => Left(e)
     case (Left(e), _) => Left(e)
     case (Right(a1), Right(b1)) => Right(f(a1, b1))
   }
//   b flatMap (aa => b map (bb => f(aa, bb)))
 }

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a1 <- this;
      b1 <- b
    } yield f(a1,b1)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil))((h,t) => h.map2(t)(_ :: _))
  }

  def sequence1[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((h,t) => f(h).map2(t)(_ :: _))
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]