package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree:Tree[A]):Int = 
    tree match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r)
    }

  def max(a:Int,b:Int) = if (a > b) a else b

  def maximum(tree:Tree[Int]):Int = 
    tree match {
      case Leaf(x) => x
      case Branch(l,r) => max(maximum(l), (maximum(r)))
    }

  def depth(tree:Tree[Int]):Int = 
    tree match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + max(depth(l), depth(r))
    }

  def map[A,B](tree:Tree[A])(f:(A) => B):Tree[B] = 
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A,B](tree:Tree[A], z:B)(f:(B, A) => B):B = 
    tree match {
      case Leaf(x) => f(z, x)
      case Branch(l, r) => fold(r, fold(l, z)(f))(f)
    }

  def maximumFold(tree:Tree[Int]) =
    fold(tree, 0)((b,a) => max(b,a))
}
