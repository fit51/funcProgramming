package errorHandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(b => f(a, b)))
}
case class Left[+E](v: E) extends Either[E, Nothing]
case class Right[+A](v: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case x :: xs => x.map2(sequence(xs))((v, vv) => v :: vv)
    case Nil => Right(Nil)
  }

}

object Tasks2 {
  def Try[A](a: => A): Either[Exception, A] = try {
    Right(a)
  } catch {case e: Exception => Left(e)}
}

object Run2 extends App {
  import Either._
  import Tasks2._

  val r1 = Try(10/2)
  val r2 = Try(10/5)
  val l1 = Try(10/0)
  val l2 = Try{throw new Exception("Error!")}
  println(r1.map(_ + 2))
  println(l1.map(_ + 2))

  for {
    a <- Try { 10/ 2}
  } yield println(a)

  println(sequence(List(r1, r1)))
  println(sequence(List(r1, l1, l2, r1)))
}

