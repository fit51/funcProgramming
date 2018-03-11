package laziness

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, tl) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, tl) => h() :: tl().toList
  }

  def take(n: Int): Stream[A] = if (n > 0) this match {
    case Empty => Empty
    case Cons(h, t) => cons(h(), t().take(n - 1))
  } else Empty

  def drop(n: Int): Stream[A] = if(n > 0) this match {
    case Empty => this
    case Cons(_, t) => t().drop(n-1)
  } else this

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>  {
      val eval = h()
      if(p(eval))
        cons(eval, t().takeWhile(p))
      else
        Empty
    }
  }
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  //f take second parameter by name and may choose not to eva luate it
  def foldRight[B](z: B)(f:(A, =>B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }
  def exists_1(p: A => Boolean): Boolean =
    foldRight(false) ((x, z) => p(x) ||  z)
  def exists_2(p: A => Boolean): Boolean =
    foldRight(false) ((x, z) => z || p(x))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, z) => p(x) && z)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, z) =>
      if (p(x)) cons(x, z) else empty
    )

  def headOption_1(): Option[A] = {
    foldRight[Option[A]](None)((x, _) => Some(x))
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) =>
      cons(f(h), t)
    )

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, t) =>
      if(f(h)) cons(h, t) else t
    }
  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

  def map_unfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }
  def take_unfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
    case _ => None
  }
  def zipWith[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
    case _ => None
  }
  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, s) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case (Empty, Empty) => None
  }
  def startsWith[A](s: Stream[A]): Boolean = {
    ???
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(prev: Int, cur: Int): Stream[Int] = cons(prev, loop(cur, prev + cur))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((v, s)) => cons(v, unfold(s)(f))
  }

  def constant_unfold(n: Int): Stream[Int] = unfold(n)(s => Some(n, s))
  def from_unfold(n: Int): Stream[Int] = unfold(n)(s => Some(n, n+1))
  def fibs_unfold: Stream[Int] = unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
}

object Run extends App {
  import Stream.cons

  def mayBeTwice(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }
  mayBeTwice(true, { println("hi"); 1 + 41})


  def s1 = {
    //3 and 5 have side effects
    cons(1, cons(2, cons( {println("str"); 3}, cons(4, cons({println("str"); 5}, cons(6, Stream.empty))))))
    //    Stream[Int](1, 2, {println("str"); 3}, 4, {println("str"); 5})
  }
  def s2: Stream[Double] = {
    cons(7.0, cons({println("str"); 8.0}, Stream.empty))
  }

  println("Evaluate all")
  println(s1.toList)

  println("Take 3")
  println(s1.take(3))
  println("Evaluate 3")
  println(s1.take(3).toList)

  println("Drop 5")
  println(s1.drop(5).toList)

  println("Take while < 4")
  println(s1.takeWhile(_ < 4))
  println(s1.takeWhile(_ < 4).toList)

  println("FoldLeft Start")
  println(s1.exists_1(x => x == 4))
  println(s1.exists_2(x => x == 4))
  println(s1.takeWhile_1(_ < 2).toList)
  println(s1.headOption)

  println("Map, filter, append")
  println(s1.map(_ * 2).take(2).toList)
  println(s1.filter(_ % 2 == 0).take(1).toList)
  println(s1.append(s2).toList)
  println(s1.flatMap(x => cons(x, cons(x+1, Stream.empty))).toList)

  println("Generators")
  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.take(2).toList)
  println(Stream.constant(3).take(2).toList)
  println(Stream.from(2).take(4).toList)
  println(Stream.fibs.take(7).toList)
  println(Stream.fibs_unfold.take(7).toList)

  println("Via Unfold")
  println(s1.map_unfold(_ + 2).take(2).toList)
  println(s1.zipWith(s2).toList)
  println(s1.take_unfold(2).toList)
  println(s1.zipAll(s2).toList)

}