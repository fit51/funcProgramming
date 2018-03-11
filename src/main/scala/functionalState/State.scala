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
  import Machine._
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.foldLeft(unit((0, 0))) {
      case (s, Coin) => s.flatMap(_ => coin)
      case (s, Turn) => s.flatMap(_ => turn)
    }
  }

}

object Machine {
  type SMachine[T] = State[Machine, T]

  def unit(v: (Int, Int)): SMachine[(Int, Int)] =
    State(m => (v, m))

  def coin: SMachine[(Int, Int)] = State((m: Machine) =>
    if (m.candies <= 0 || !m.locked) {
      ((m.coins, m.candies), m)
    } else {
      ((m.candies, m.coins + 1), Machine(false, m.candies, m.coins + 1))
    })

  def turn: SMachine[(Int, Int)] = State((m: Machine) =>
    if (m.locked)
      ((m.coins, m.candies), m)
    else
      ((m.coins, m.candies - 1), Machine(true, m.candies - 1, m.coins))
  )
}

object Candy {
  import State._
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
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

  val m = Machine(true, 5, 10)
  val actions = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val r = m.simulateMachine(actions).run(m)._1
  println(r)
  println("hello")
}
