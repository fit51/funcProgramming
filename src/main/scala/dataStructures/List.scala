package dataStructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](xs: A*): List[A] = {
    if(xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
  }

  //1
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Nil does not have tail")
    case Cons(x, xs) => xs
  }
  def setHead[A](l: List[A], head: A) = Cons(head, tail(l))
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n <= 0 => l
    case Nil => throw new Exception("Nil does not have tail")
    case Cons(x, xs) => drop(xs, n-1)
  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }
  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }
  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  }
  //2
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)
  def product(ds: List[Int]): Int = foldRight(ds, 1)((x, y) => x * y)
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => 1 + y)
  //3
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
    case Nil => z
  }
  def reverse[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil)(Cons(_,_))
  //3

  def foldRightTailRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)(f)
  }

  def append[A](l: List[A], r: List[A]): List[A] = l match {
    case Nil => r
    case Cons(x, xs) => Cons(x, append(xs, r))
  }

  def concat[A](l: List[List[A]]) = foldLeft[List[A], List[A]](l, Nil)((x, y) => append(x, y))

  def appendRight[A](l: List[A], r: List[A]): List[A] = foldRightTailRec(l, r)(Cons(_, _))
  def appendLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)(Cons(_, _))

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRightTailRec[A, List[B]](as, Nil)((x, y) => Cons(f(x), y))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRightTailRec[A, List[A]](as, Nil)((x, y) => if (f(x)) Cons(x, y) else y)
  }
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRightTailRec[A, List[B]](as, Nil)((x, y) => appendLeft[B](y, f(x)))
  }
}


object Runner extends App {
  import List._

  List(1, 2, 3) match {
    case Cons(x, _) => println(x)
  }

  val l = List(1, 2, 4, 5)
  //1
  println(List.drop(l, 4))
  println(List.dropWhile[Int](l, _ < 3))
  println(List.dropWhileCurried(l)(_ < 3))
  //2
  println(List.product(l))
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(List.foldLeft(l, 0)(_ + _))
  println(reverse(l))
  //3
  println(List.foldRightTailRec(l, 3)((x, y) => if(y > x) x + y else y - x))
  println(List.append(l, List(6, 7)))
  println(List.foldLeft(l, 3)((x, y) => if(y > x) x + y else y - x))
  println(map(l)(_ + 2))
  println(filter(l)(_ % 2 == 0))
  println(flatMap(l)(x => Cons(x, Cons(x + 1, Nil))))
  //4
  val l2 = List(1, 3)
  println(concat(List(l2, l)))
}
