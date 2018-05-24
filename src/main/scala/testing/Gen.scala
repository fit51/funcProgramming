package testing

import laziness.Stream
import functionalState._
import parallelism._
import parallelism.Par.Par
import Gen._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop { self =>
  import Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      ???
//    self.check && p.check
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

//Intermidiate Impl
case class GenS[A](sample: State[RNG, A])

object GenS {
  def unit[A](a: => A): GenS[A] = GenS(State(s => (a, s)))
  def boolean: GenS[Boolean] = GenS(State(
    SimpleRNG.map(SimpleRNG.int)(_ % 2 == 0)
  ))
  def listOfN[A](n: Int, g: GenS[A]): GenS[List[A]] = {
    GenS(
      State(
        SimpleRNG.sequence(List.fill(n)(g.sample.run))
      )
    )
  }
  def choose(start: Int, stopExclusive: Int): GenS[Int] = {
    GenS(State(
      SimpleRNG.map(
        SimpleRNG.nonNegativeLessThan(stopExclusive - start)
      )(
        _ + start
      )
    ))
  }
}


object Gen {
  def unit[A](a: => A): Gen[A] = ???
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}