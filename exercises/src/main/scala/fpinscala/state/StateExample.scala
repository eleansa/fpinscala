package fpinscala.state

/**
 * Created by ele on 29/04/2014.
 */
object StateExample extends App {

  import RNG._

  val rng = Simple(1L)

  val x1 = nonNegativeInt(rng)
  val x2 = nonNegativeInt(x1._2)
  val x3 = nonNegativeInt(x2._2)
  val x = List(x1,x2,x3)

  x.foreach(a => println(s"nonNegativeInt:  ${a}"))

  val d1 = double(rng)
  val d2 = double(d1._2)
  val d3 = double(d2._2)
  val d = List(d1,d2,d3)

  d.foreach(a => println(s"double:          ${a}"))

  println(s"Ints, RNG:       ${ints(4)(rng)}")

  println(s"doubleElegant:   ${doubleElegant(rng)}")

  println(s"IntDouble X1:    ${x1}")
  println(s"IntDouble X1:    ${double(x1._2)}")
  println(s"IntDouble Combo: ${randIntDouble(rng)}")

  println(s"DoubleInt X2:    ${d1}")
  println(s"DoubleInt X2:    ${double(d1._2)}")
  println(s"DoubleInt Combo: ${randDoubleInt(rng)}")

  val fs: List[Rand[Int]] = List(nonNegativeInt, nonNegativeEven)
  println(s"Sequence:        ${sequence(fs)(rng)}")

  println(s"FlatMap:         ${flatMap(nonNegativeEven)(int => rng => nonNegativeInt(rng))(rng)}")

//  val result = for{
//    (a, r1) <- nonNegativeEven(rng)
//    (b, r2) <- nonNegativeInt(r1)
//  } yield (a,b,r2)

}
