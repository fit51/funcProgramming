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

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](f: => A): Option[A] =
    try Some(f)
    catch { case e: Exception => None }

  def map2[A, B, C](o1: Option[A], o2: => Option[B])(f: (A, B) => C): Option[C] = {
    o1.flatMap(v1 => o2.map(v2 => f(v1, v2)))
  }

  def sequence_1[A](l: List[Option[A]]): Option[List[A]] = {
    def loop(ll: List[Option[A]]): List[A] = ll match {
      case Some(v) :: t => v :: loop(t)
      case _ => Nil
    }
    val r = loop(l)
    if(r.length == l.length)
      Some(r)
    else
      None
  }

  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case Some(v) :: t => sequence(t) map (v :: _)
    case None :: _ => None
  }

  def traverse[A, B](l: List[A], f: (A) => Option[B]): Option[List[B]] = l match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t, f) map (hh :: _))
  }

}
object Run extends App {
  import Tasks._

  println(Some(1).map(_ * 2))
  println(Some(11).filter(_ % 2 == 0))

  //2
  val absO: Option[Double] => Option[Double] = lift(math.abs)

  //3
  println(map2(Try {"123".toInt}, Try {"32".toInt})(_ + _))
  println(map2(Try {"sdd".toInt}, Try {"32".toInt})(_ + _))
  println(map2(Try {"123".toInt}, Try {"sd".toInt})(_ + _))

  //4
  println(sequence(List(Some(1), Some(2))))
  println(sequence(List(Some(1), None, Some(2))))
  println(traverse[String, Int](List("1", "23", "23"), x => Try{x.toInt}))
  println(traverse[String, Int](List("1", "23ds", "23"), x => Try{x.toInt}))
}