package state

import scala.collection.mutable._

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[A, B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a,b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State( s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, S] = State(s => (s,s))

  def modify[S](f: S => S): State[S, Unit] = {
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

}

object State {

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = {
    ss.foldRight(unit[S, List[A]](List())){ (elem, acc) =>
      elem.map2(acc)((a,b) => a :: b)
    }
  }

}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = {
    rng =>
      val (a, r) = s(rng)
      (f(a), r)
  }

  def map2[A, B, C](ran1: Rand[A], ran2: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, r) = ran1(rng)
      val (b, r2) = ran2(r)
      (f(a, b), r2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val  (a, r) = f(rng)
      g(a)(r)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(x => unit(f(x)))
  }

  def map2ViaFlatMap[A, B, C](ran1: Rand[A], ran2: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ran1)(a => map(ran2)(b => f(a,b)))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      fs.foldRight((List.empty[A], rng)){ (elem, acc) =>
        val x = elem(acc._2)
        (x._1 :: acc._1, x._2)
      }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def positiveIntUsingFlatMap = {
    flatMap(int){ i =>
      if (i != Int.MinValue) unit(i) else int
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val max = Int.MaxValue + 1
    (i.toDouble / max, r)
  }

  def _double(rNG: RNG) = {
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val res = new ListBuffer[Int]

    def loop(n: Int, rng: RNG): (List[Int], RNG) = {
      if(n == 0) (res.toList, rng)
      else {
        val (i, r) = rng.nextInt
        res += i
        loop(n-1, r)
      }
    }
    loop(count, rng)
  }

  def intsViaSequence(count: Int) = {
    sequence(List.fill(count)(int))
  }

}

object RNGOps extends App {
  import RNG._

  val s = Simple(1L)
  val rng = new RNG {
    override def nextInt: (Int, RNG) = s.nextInt
  }

//  val nn = nonNegativeInt(rng)

//  val x = unit(1)(rng)
//  val y = unit(2)(rng)
//  val z = unit(3)(rng)
//  val rs = sequence(List(unit(1), unit(2), unit(3)))(rng)._1
//  println(rs)





}
