package fpinscala.datastructures
import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = 
    l match { 
      case Cons(_,xs) => xs
      case Nil => Nil
    }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = 
    l match {
      case Cons(x,xs) => if (n <= 0) Cons(x,xs) else drop(xs, n-1)
      case Nil => Nil
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs,f) else Cons(x,xs)
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x,xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,s) => 1 + s)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }
  def sum3(ns:List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns:List[Int]) = foldLeft(ns, 1)(_ * _)
  
  def length3[A](l: List[A]): Int = foldLeft(l, 0:Int)((s,_) => 1 + s)

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldLeft(reverse(l), Nil:List[B])((b, a) => Cons(f(a), b))

  def reverse[A](xs:List[A]) = foldLeft(xs, Nil:List[A])((b,a) => Cons(a, b))

  def foldRightWithFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    List.foldLeft(List.reverse(l), z)((b,a) => f(a,b))
  }
  def appendWithFoldLeft[A](xs:List[A], ys:List[A]) = {
    List.foldLeft(List.reverse(xs), ys)((b,a) => Cons(a,b))
  }

  def concat[A](ls:List[List[A]]):List[A] = 
    foldLeft(ls, Nil:List[A])((b,a) => append(b,a))

  def ex3_16(ls:List[Int]) = map(ls)(_ + 1)

  def ex3_17(ls:List[Double]) = map(ls)(_.toString)

  def filter[A](as:List[A])(f: A => Boolean):List[A] = {
    as match {
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
      case xs => xs
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))

  def filterWithFlatMap[A](as:List[A])(f: A => Boolean):List[A] = 
    flatMap(as)(a => if (f(a)) Cons(a,Nil) else Nil:List[A])

  def zip(as:List[Int])(bs:List[Int]):List[(Int,Int)] = 
    (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons((x,y), zip(xs)(ys))
    case _ => Nil
  }

  def ex3_22(as:List[Int], bs:List[Int]):List[Int] = {
    val zs = List.zip(as)(bs)
    map(zs)((xy:(Int,Int)) => xy._1 + xy._2)
  }

  def zipWith[A,B](as:List[A])(bs:List[A])(f:(A,A) => B):List[B] = {
    (as, bs) match {
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), zipWith(xs)(ys)(f))
      case _ => Nil
    }
  }
  def hasSubsequence[A](sup: List[A], sub:List[A]):Boolean = {
    sup match {
      case Nil => false
      case Cons(x,xs) => {
        val zs = zipWith(Cons(x,xs))(sub)((x,y) => x == y)
        val here = foldLeft(zs, true)((b,a) => b && a)
        here || hasSubsequence(xs, sub)
      }
    }
  }
}
