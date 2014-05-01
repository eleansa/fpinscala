package fpinscala.state

/**
 * Created by ele on 01/05/2014.
 *
 * - Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
 * - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
 * - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
 * - A machine that is out of candy ignores all inputs.
 */
import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      })))
      s <- get
    } yield (s.coins, s.candies)
}



object CandyExample extends App {

  import Candy._

  val machine = Machine(locked = true, candies = 5, coins = 10)

  val instructions = List(Coin, Coin, Turn)

  println(s"SimulateMachine:  ${simulateMachine(instructions)}")
}

