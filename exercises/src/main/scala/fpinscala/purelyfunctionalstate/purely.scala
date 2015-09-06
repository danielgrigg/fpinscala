package fpinscala.purely

trait RNG { 
  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Purely {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = 
    map2(ra, rb)((_, _))


  def sequence[A](rs:List[Rand[A]]): Rand[List[A]] = {
    rng => {
      rs.foldLeft((List[A](), rng)) { case ((l,rng2), r) => {
        val (y, rng3) = r(rng2)
        (y::l, rng3)
      }
      }
    }
  }


  def nonNegativeInt(rng:RNG): (Int, RNG) =  {
    val (n, r2) = rng.nextInt
    (((n.toLong + Int.MaxValue + 1) % Int.MaxValue).toInt, r2)
  }

  def double(rng:RNG): (Double, RNG) = {
    val (n, r2) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue + 1.0), r2)
  }

  def intDouble(rng:RNG): ((Int, Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i,d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r2) = intDouble(rng)
    ((d,i),r2)
  }

  def double3(rng:RNG):((Double,Double,Double), RNG) = {
    val (d, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d, d2, d3), r4)
  }

  def ints(count:Int)(rng:RNG): (List[Int], RNG) = {
    def iter(n:Int, acc: List[Int], r:RNG):(List[Int], RNG) = 
      if (n > 0) { 
        val (i, rNext) = r.nextInt
        iter(n-1, i::acc, rNext)
      } else {
        (acc, r)
      }
    iter(count, List(), rng)   
  }

  def intsViaSequence(count:Int)(rng:RNG): (List[Int], RNG) = {
    val f = sequence(List.fill(count)((r:RNG) => r.nextInt))
    f(rng) 
  }


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleViaRand: Rand[Double] = 
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1.0))

}
