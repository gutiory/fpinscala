package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // EX 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // EX 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // EX 3.27
  def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  // EX 3.28
  def map[A, B](t: Tree[A])(f: A => B) : Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // EX 3.29
  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B)=> B) : B = t match {
    case Leaf(value) => l(value)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizeFold[A](t: Tree[A]) : Int = fold(t)(a => 1)((a,b) => 1 + a + b)

  def maximumFold(t: Tree[Int]) : Int = fold(t)(a => a)((a,b) => (a max b))

  def depthFold[A](t: Tree[A]) : Int = fold(t)(a => 0)((a,b) => 1 + (a max b))

  def mapFold[A, B](t: Tree[A])(f: A => B) : Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((a,b) => Branch(a,b))
}