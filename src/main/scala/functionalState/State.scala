package functionalState


case class State[S,+A](run: S => (A,S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (r, sNew) = run(s)
    f(r).run(sNew)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List()))((as, ls) => as.map2(ls)(_ :: _))
  }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object SimpleRand {
  import State._

  val int: Rand[Int] = State((s: RNG) => s.nextInt)
  def ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(State((s: RNG) => s.nextInt)))
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.map()
  }

}

object Machine {
  def unit(m: Machine): State[Machine, (Int, Int)] =
    State(s => ((m.coins, m.candies), m))

  def coin(m: State[Machine, (Int, Int)]): State[Machine, (Int, Int)] =
    m.map()
}

object StateRun extends App {
  import SimpleRand._
  import State._

  val ns: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield {
    xs.map(_ % y)
  }
//  println(ns.run(new SimpleRNG(13))._1)
}
