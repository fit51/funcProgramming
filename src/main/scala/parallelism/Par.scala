package parallelism

import java.util.concurrent._

import parallelism.Par.sequenceBalanced

import scala.concurrent.duration.Duration

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map2WithTimeOuts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(timeout: Duration): Par[C] = {
    def measureTime[A](f: => A): (A, Long) = {
      def timeMillis = Math.round(System.nanoTime() / 1e6)
      val start = timeMillis
      val a = f
      (a, timeMillis - start)
    }
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      val (afRes, aTime) = measureTime(af.get(timeout.toMillis, TimeUnit.MILLISECONDS))
      val timeExceeded = timeout.toMillis - aTime
      UnitFuture(f(afRes, bf.get(timeExceeded, TimeUnit.MILLISECONDS)))
    }
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit())((a, _) => f(a))

  def sequence_simple[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List.empty[A])) {
      (pA, pL) => map2(pA, pL)(_ :: _)
    }
  }

  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(List.empty)
    case h :: l => map2(h, fork(sequenceRight(l)))(_ :: _)
  }

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if(ps.length == 1) map(ps.head)(Vector(_))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF[A, Option[A]](a => if (f(a)) Some(a) else None)(_))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val nV = run(es)(n).get
      choices(nV)(es)
    }



  //----Examples
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def reducePar[A](in: IndexedSeq[A])(f: (A, A) => A): Par[A] = fork {
    if (in.length == 1) unit(in.head)
    else {
      val (l, r) = in.splitAt(in.length / 2)
      map2(reducePar(l)(f), reducePar(r)(f))(f)
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  def sumViaReduce(ints: IndexedSeq[Int]) = reducePar(ints)(_ + _)
}

private case class UnitFuture[A](v: A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  override def isCancelled: Boolean = false
  override def isDone: Boolean = true
  override def get(): A = v
  override def get(timeout: Long, unit: TimeUnit): A = v
}

object Runner extends App {
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  val lP = List(unit(1), unit(2), unit(3), unit(4))
  val pL = sequence(lP)
  println(pL)

}
