package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList : List[A] = foldRight(List[A]())((a, b) => a :: b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if(n == 0) empty else cons[A](h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (n == 0) this else t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) =>
      if(p(h())) cons[A](h(), t().takeWhile(p))
      else empty

  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhileFoldRight(p: A => Boolean) : Stream[A] =
    foldRight(Stream[A]())((h,t) => if(p(h)) cons(h,t) else empty)

  def headOption: Option[A] =
    foldRight(None:Option[A]) ((h, t) => if(h == empty) None else Some(h))

  def map[B](f: A => B) : Stream[B] =
    foldRight(Stream[B]())((h, t) => cons[B](f(h), t))

  def filter(f: A => Boolean) : Stream[A] =
    foldRight(Stream[A]())((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](e: => Stream[B]) : Stream[B] =
    foldRight(e)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight(Stream[B]())((h, t) => f(h).append(t))

  def mapUnfold[B](f: A => B) : Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()),t()))
    case _ => None
  }

  def takeUnfold(n: Int) : Stream [A] = unfold(this, n){
    case (Cons(h, t), cont) => if(cont == 0) None else Some((h() , (t(), cont - 1)))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean) : Stream[A] = unfold(this){
    case Cons(h, t) if(p(h())) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](b : Stream[B])(f: (A,B) => C) : Stream[C] = unfold(this, b) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s2){
    case (Empty, Empty) => None
    case (Empty, Cons(hb, tb)) => Some( (None, Some(hb())), (empty, tb()))
    case (Cons(ha, ta), Empty) => Some( (Some(ha()), None), (ta(), empty))
    case (Cons(ha, ta), Cons(hb, tb)) => Some( (Some(ha()), Some(hb())), (ta(), tb()))

  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).filter{
    case (x,y) => x != None && y != None
    }.forAll{
      case (a,b) => a == b
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(empty)


  def hasSubsequence[A](s :Stream[A]) : Boolean = tails.exists(_.startsWith(s))

  def scanRight[B](z: => B)(f: (A, => B) => B) :Stream[B] = ???

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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def loop(n_1: Int, n_2: Int): Stream[Int] = cons(n_2, loop(n_1 + n_2, n_1))
    loop(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    if (f(z) == None) empty
    else cons(f(z).get._1, unfold(f(z).get._2)(f))


  def onesUnfold(): Stream[Int] = unfold(1)((x => Some(1, 1)))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)((x => Some(x, x)))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibsUnfold() = unfold((0, 1)) { case (n_1, n_2) => Some((n_1, (n_2, n_1 + n_2))) }

}
