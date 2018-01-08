package errorHandling

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => Some(v)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap { x => if (f(x)) Some(x) else None }
  }

  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => Some(v)
    case _ => None
  }
}

object Tasks {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.length < 1) None
    else Some(xs.reduce(_ + _) / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap { m =>
    mean(xs.map(x => math.pow(x - m, 2)))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}
object Run extends App {
  import Tasks._

  println(Some(1).map(_ * 2))
  println(Some(11).filter(_ % 2 == 0))

  //2
  val absO: Option[Double] => Option[Double] = lift(math.abs)
}