package fpinscala.laziness

import Stream._
trait Stream[+A] {

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

  def toList:List[A] = this match {
    case Cons(h, t) => h()::(t().toList)
    case _ => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n-1)) else empty
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n-1) else Cons(h,t)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else empty
    case _ => empty
  }
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty[A])
    
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = 
    foldRight(None:Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B):Stream[B] = 
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(f: A => Boolean):Stream[A] = 
    foldRight(empty[A])((a,b) => if (f(a)) cons(a, b) else b)

//  def append(s2: => Stream[A]):Stream[A] = foldRight(s2)((a,b]) => cons(a,b))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def mapViaUnfold[B](f: A => B):Stream[B] = 
    unfold(this)(s => s match {
      case Cons(x, xs) => Some((f(x()), xs()))
      case empty => None
    })

  def takeViaUnfold(n: Int): Stream[A] = 
    unfold((n,this))(s => s match {
      case ((m,Cons(x, xs))) if m > 0  => Some(x(), (m-1, xs()))
      case _ => None
    })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = 
    unfold(this)(s => s match {
      case Cons(x, xs) if p(x()) => Some(x(), xs())
      case _ => None
    })

  // contraviant nonsense
  def zipWithViaUnfold[B,C](s2:Stream[B])(f:(A,B) => C):Stream[C] = 
    unfold((this, s2))(s => s match {
      case (Cons(x,xs), Cons(y,ys)) => Some((f(x(),y()), (xs(),ys())))
      case _ => None
    })

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
      unfold((this, s2))(s => s match {
        case (Empty, Empty) => None
        case (Empty, Cons(y,ys)) => Some((None, Some(y())), (Empty, ys()))
        case (Cons(x,xs), Empty) => Some((Some(x()), None), (xs(), Empty))
        case (Cons(x,xs), Cons(y,ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
      })
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

  def fibsn(a:Int, b:Int):Stream[Int] = 
    Stream.cons(a+b, fibsn(b, a+b))

  val fibs:Stream[Int] = Stream.cons(0, Stream.cons(1, fibsn(0,1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((value,state)) => Stream.cons(value, unfold(state)(f))
      case None => empty
    }

  val onesUsingUnfold:Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantUsingUnfold[A](a:A): Stream[A] = unfold(a)(s => Some(s, s))

  def fromUsingUnfold(n: Int): Stream[Int] = 
    unfold(n)(s => Some(s, s+1))

  def fibsnUsingUnfold(a:Int, b:Int):Stream[Int] =
    unfold((a,b))(xy => Some(xy._1 + xy._2, (xy._2, xy._1+xy._2)))

  val fibsUsingUnfold:Stream[Int] = 
    Stream.cons(0, Stream.cons(1, fibsnUsingUnfold(0, 1)))

}
