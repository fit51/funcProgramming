package functionalState

trait RNG {

  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val res = if (n <= 0)
      math.abs(n + 1)
    else n
    (res, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n.toDouble/(Int.MaxValue + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0) {
      (Nil, rng)
    } else {
      val (n, rng2) = rng.nextInt
      val (l, lrng) = ints(count - 1)(rng2)
      (n :: l, lrng)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleComp: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble/Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) =>
      map2(f, acc)(_ :: _)
    )
  }

  def intsComp(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  def mapFromFlatMap[A, B](s: Rand[A])(f: A => B) =
    flatMap(s) { a => unit(f(a)) }

  def map2FromFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

object Runner extends App {

  import SimpleRNG._

  println(Int.MaxValue)
  println(Int.MinValue)
  //Standart
  val Srng = new scala.util.Random
  println(Srng.nextInt)
  println(Srng.nextInt)
  println(Srng.nextInt)

  //Functional
  println("Functional")
  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  println(n1)

  val (n2, rng3) = nonNegativeInt(rng2)
  println(n2)
  val (n3, rng4) = nonNegativeInt(rng3)
  println(n3)
  val (n4, rng5) = double(rng4)
  println(n4)
  val (l, rng6) = ints(10)(rng5)
  l.foreach(x => print(s" $x "))
  println()
  val (n5, rng7) = intDouble(rng6)
  println(n5)

  println("Using Rang type")
  println(int(rng)._1)
  println(nonNegativeEven(rng7)._1)
  println(doubleComp(rng7)._1)
  println(sequence(
    List.fill(10)(
      nonNegativeLessThan(10)
    ))(rng6)._1)
}