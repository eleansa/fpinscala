package fpinscala.iomonad

import Future._

object FutureTest extends App {
  val N = 100000
  def worstCaseScenario1 =
    (0 to N).map(i => Future(i)).foldLeft(Future(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield (acc + i)
    }
  def worstCaseScenario1a =
    (0 to N).map(i => Throw.more(Throw.unit(i))).foldLeft(Throw.unit(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield (acc + i)
    }
  def worstCaseScenario2 =
    (0 to N).map(i => delay(i)).foldLeft(delay(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield (acc + i)
    }
  def worstCaseScenario2a =
    (0 to N).map(i => delay(i)).foldLeft(now(0)) {
      (f1,f2) => for {
        acc <- f1
        i <- f2
      } yield (acc + i)
    }

//FIXME: Doesn't work
//  def worstCaseScenario3 =
//    (0 to N).map(i => Future(i)).foldLeft(now(0)) {
//      (f1,f2) => for {
//        acc <- f1
//        i <- f2
//      } yield (acc + i)
//    }
//  def worstCaseScenario3a =
//    (0 to N).map(i => Future(i)).foldLeft(delay(0)) {
//      (f1,f2) => for {
//        acc <- f1
//        i <- f2
//      } yield (acc + i)
//    }
//  def runRepeatedly(n: Int): Future[Unit] =
//    if (n > 0) Future { () } flatMap (_ => runRepeatedly(n-1))
//    else now(())
  
  println(worstCaseScenario1.run)
  println(worstCaseScenario1a.run)
//  println(worstCaseScenario2.run)
//  println(worstCaseScenario2a.run)
//  println(worstCaseScenario3.run)
//  println(worstCaseScenario3a.run)
//  runRepeatedly(10000).run
}

