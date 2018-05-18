package parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Nonblocking {

  sealed trait Future[+A] {
    //k - callback function
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
    val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
    p(es) { a => try { ref.set(a) } catch { case e => println(e) } finally { latch.countDown } } // Asynchronously set the result, and decrement the latch
    latch.await // Block until the `latch.countDown` is invoked asynchronously
    ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = try { r } catch { case e => println(e)} })

  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
    es => new Future[C] {
      var ar: Option[A] = None
      var br: Option[B] = None
      def apply(cb: C => Unit): Unit = {
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit())((a, _) => f(a))

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if(ps.length == 1) map(ps.head)(Vector(_))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      p(es) { b =>
        if (b) eval(es) { t(es)(cb) }
        else eval(es) { f(es)(cb) }
      }
  }

  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      p(es) { b =>
        eval(es) { ps(b) }
      }
  }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    ???

  def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
    ???

  // see `Nonblocking.scala` answers file. This function is usually called something else!
  def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    ???

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    ???

  def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    ???

  def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    ???

  def join[A](p: Par[Par[A]]): Par[A] =
    ???

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    ???

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    ???

}

object Runner2 extends App {
  import java.util.concurrent.Executors
  import Nonblocking._

  val p = parMap(List.range(1, 10000))(math.sqrt(_))
  val tP = Executors.newFixedThreadPool(2)

  val x = run(tP)(p)
  println(x)

  //Error
  val pFail = lazyUnit[Int](throw new Exception("Ha ha!"))
  val pOk = lazyUnit[Int](1 + 2 + 3)
  val res = run(tP) {
    map2(pOk, pFail)(_ + _)
  }
  println(res)
  tP.shutdown()
  println("Finish!")
}
