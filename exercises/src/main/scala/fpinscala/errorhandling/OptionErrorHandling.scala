package fpinscala.errorhandling

object OptionErrorHandling {

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def populationVariance(xs: Seq[Double]): Option[Double] = {
    val mOption = mean(xs)
    mOption flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


    //    val m: Double = mean(xs).getOrElse(0.0)
//    val seq: Seq[Double] = xs.map(x => math.pow(x - m, 2))
//    mean(seq)
  }

  def sampleVariance(xs: Seq[Double]): Option[Double] = {
    val mOpt = mean(xs)
    mOpt flatMap(m => {
      val seq: Seq[Double] = xs.map(x => math.pow(x - m, 2))
      Some(seq.sum / (seq.length - 1))
    })

//    val m: Double = mean(xs).getOrElse(0.0)
//    val seq: Seq[Double] = xs.map(x => math.pow(x - m, 2))
//    Some(seq.sum / (seq.length - 1))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (_, None) => None
      case (None, _) => None
      case (Some(a1), Some(b1)) => Some(f(a1, b1))
    }
  }

  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }


  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h,t) => map2(h,t)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
  }

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}

object ErrorHandlingExample extends App {

  val seq1 = Seq(1.0,2.0,3.0,4.0,5.0)
  val seq2 = Seq(1.0,2.0,4.0,5.0,6.0)
  val seq3 = Seq(1.0, 2.0, 9.0)
  val optNone: Option[Int] = None

  println(s"Population Variance: Expected 2.0   === ${OptionErrorHandling.populationVariance(seq1)}")
  println(s"Population Variance: Expected 3.44  === ${OptionErrorHandling.populationVariance(seq2)}")
  println(s"Population Variance: Expected 12.66 === ${OptionErrorHandling.populationVariance(seq3)}")

  println(s"Sample Variance:     Expected 2.5   === ${OptionErrorHandling.sampleVariance(seq1)}")
  println(s"Sample Variance:     Expected 4.3   === ${OptionErrorHandling.sampleVariance(seq2)}")
  println(s"Sample Variance:     Expected 19.0  === ${OptionErrorHandling.sampleVariance(seq3)}")

  println(s"map2:                Expected 3     === ${OptionErrorHandling.map2(Some(2), Some(1))(_ + _)}")
  println(s"map2:                Expected None  === ${OptionErrorHandling.map2(optNone, Some(1))((a,b) => a+b)}")
  println(s"map2:                Expected None  === ${OptionErrorHandling.map2(Some(1), optNone)((a,b) => a+b)}")
  println(s"map2:                Expected None  === ${OptionErrorHandling.map2(optNone, optNone)((a,b) => a+b)}")
}