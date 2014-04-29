package fpinscala.laziness

/**
* Created by ele on 28/04/2014.
 */
object StreamExample extends App {

  val stream1 = Stream(1,2,3,4,5)
  val stream2 = Stream(7,8,9)
  val stream3 = Stream(1,2,3)

  import Stream._

  val streamEmpty = empty

  val ones:Stream[Int] = cons(1, ones)

  println(s"toList:       ${stream1.toList}")
  println(s"drop:         ${stream1.drop(2).toList}")
  println(s"take:         ${stream1.take(2).toList}")
  println(s"takeWhile:    ${stream1.takeWhile(_ < 4).toList}")
  println(s"takeWhileFold:${stream1.takeWhileFold(_ < 4).toList}")
  println(s"forAll:       ${stream1.forAll(_ < 4)}")
  println(s"forAll:       ${stream1.forAll(_ < 6)}")
  println(s"headOption:   ${stream1.headOption}")
  println(s"headOption:   ${streamEmpty.headOption}")
  println(s"map + 1:      ${stream1.map(_ + 1).toList}")
  println(s"filter :      ${stream1.filter(_ < 3).toList}")
  println(s"append :      ${stream1.append(stream2).toList}")
  println(s"flatMap :     ${stream1.flatMap(i => cons(i, cons(i + 2, empty))).toList}")

  println(s"Infinite ones: ${ones.take(2).toList}")
  println(s"Constant:      ${constant(3).take(3).map(_ * 3).toList}")
  println(s"From:          ${from(3).take(3).toList}")
  println(s"Fibs 10:       ${fibs.take(10).toList}")
  println(s"Fibs 1:        ${fibs.take(1).toList}")
  println(s"Fibs 2:        ${fibs.take(2).toList}")
  println(s"Fibs 3:        ${fibs.take(3).toList}")

  println(s"Unfold:        ${unfold(3)(x => Some((x, x+1))).take(3).toList}")

  println(s"Fibs Unfold 10:${fibsViaUnfold.take(10).toList}")
  println(s"Fibs Unfold  1:${fibsViaUnfold.take(1).toList}")
  println(s"Fibs Unfold  2:${fibsViaUnfold.take(2).toList}")
  println(s"Fibs Unfold  3:${fibsViaUnfold.take(3).toList}")

  println(s"From Unfold:   ${fromViaUnfold(10).take(10).toList}")
  println(s"Const Unfold:  ${constantViaUnfold("a").take(10).toList}")
  println(s"Ones Unfold:   ${onesViaConstant.take(3).toList}")

  println(s"map Unfold:    ${stream1.mapViaUnfold(_ + 1).toList}")
  println(s"take Unfold:   ${stream1.takeViaUnfold(4).toList}")
  println(s"takeWhl Unfold:${stream1.takeWhileViaUnFold(_ < 4).toList}")
  println(s"zipWith Unfold:${stream1.zipWith(stream2)((x,y) => (x + "x",y)).toList}")
  println(s"zipAll Unfold: ${stream1.zipAll(stream2).toList}")
  println(s"zipAll Unfold: ${stream2.zipAll(stream1).toList}")

  println(s"startsWith:    ${stream1.startsWith(stream2)}")
  println(s"startsWith:    ${stream1.startsWith(stream3)}")
  println(s"tails:         ${stream3.tails.map(x => x.toList).toList}")

  println(s"HasSubsequent (1,2) in (1,2,3,4,5): ${stream1.hasSubsequence(Stream(1,2,3))}")
  println(s"HasSubsequent (2,3) in (1,2,3,4,5): ${stream1.hasSubsequence(Stream(2,3))}")
  println(s"HasSubsequent (4) in (1,2,3,4,5):   ${stream1.hasSubsequence(Stream(4))}")


}
