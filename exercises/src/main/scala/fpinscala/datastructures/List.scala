package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // EX 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // EX 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match  {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  // EX 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if(n == 0) l
    else drop(tail(l), n - 1)

  // EX 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if(f(x)) dropWhile(xs, f)
      else l
  }

  def init[A](l: List[A]): List[A] = sys.error("todo")

  // EX 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, z) => z + 1)

  // EX 3.10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil =>
        println(z)
        z
      case Cons(x, xs) =>
        println("foldLeft(" + xs.toString + ", f(" + x + "," + z + "))")
        foldLeft(xs,f(z,x))(f)
    }
  }

  // EX 3.11
  def sumFoldLeft(l : List[Int]) = foldLeft(l, 0)(_ + _)

  def prodFoldLeft(l : List[Double]) = foldLeft(l, 1.0)(_ * _)

  def lengthFoldLeft[A](l : List[A]) = foldLeft(l, 0)((z, x) => z + 1)

  // EX 3.12
  def reverse[A](as: List[A]) = {
    def iter(xs: List[A], acum: List[A]) : List[A] = xs match {
      case Nil => acum
      case Cons(x, xt) => iter(xt, Cons(x, acum))
    }
    iter(as, Nil)
  }

  def reverseFold[A](as: List[A]) : List[A] = foldLeft(as, List[A]()) ((h, acc) => Cons(acc, h))

  // EX 3.13
  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((x, y) => f(y, x))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f:(A, B) => B) : B = foldLeft(as, z)((x,y) => f(y,x))

  // EX 3.14
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  // EX 3.15
  def appendLists[A](as: List[List[A]]) : List[A] = {
    def iter(current: List[List[A]], acum: List[A]) : List[A] = current match {
      case Nil => acum
      case Cons(h, t) => iter(t, append(acum, h))
    }
    iter(as, Nil)
  }

  def appendListsFR[A](as: List[List[A]]) : List[A] =  foldRight(as, Nil:List[A])(append)

  // EX 3.16
  def addOne(as: List[Int]) : List[Int] = foldRight(as, List[Int]())((x, z) => Cons(x + 1, z))

  // EX 3.17
  def doublesToStrings(as: List[Double]) : List[String] = foldRight(as, List[String]())((x,z) => Cons(x.toString, z) )

  // EX 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((x, z) => Cons(f(x), z))

  // EX 3.19
  def filter[A](as: List[A])(f: A => Boolean) : List[A] = foldRight(as, List[A]())((x, z) => if(f(x)) Cons(x, z) else z)

  // EX 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = foldRight(as, List[B]())((h, t) => append(f(h), t))

  // EX 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean) : List[A] = flatMap(as)(x => if(f(x)) List(x) else List())

  // EX 3.22
  def add2ListInts(l1: List[Int], l2: List[Int]) : List[Int] = (l1, l2) match {
    case (_ , Nil) =>  Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1),Cons(h2, t2)) => Cons(h1+h2, add2ListInts(t1, t2))
  }

  // EX 3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C) : List[C] = (l1, l2) match {
    case (_ , Nil) =>  Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1),Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }

  // EX 3.24
  def hasSubsequence[A](sup: List[A], sub:List[A]) : Boolean = {
    println(sup + " ====== " + sub)
    (sup,sub) match {
      case (Nil, Nil) => true
      case (Nil,_) => false
      case (_, Nil) => true
      case (Cons(supH, supT), Cons(subH, subT)) =>
        if(supH == subH) hasSubsequence(supT, subT)
        else hasSubsequence(dropWhile(Cons(supH, supT), (x:A) => x != subH) ,Cons(subH, subT))
    }
  }
}
