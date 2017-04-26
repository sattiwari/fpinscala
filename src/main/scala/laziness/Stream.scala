package laziness

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import Stream._

trait Stream[+A] {
  def toListRecursive: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => Nil
    }
  }

  def toList: List[A] = {

    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }
    }

    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buf = new ListBuffer[A]

    @tailrec
    def go(s: Stream[A]): List[A] = {
      s match {
        case Cons(h, t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }
    }

    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n-1))
      case _ => None
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if(p(h())) => Some((h(), t()))
      case _ => None
    }
  }

  //  using foldRight
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, b) =>
      if(p(h)) cons(h, b)
      else empty)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forall(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def map[B](p: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(p(a), b))
  }

  def mapViaUnfold[B](p: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((p(h()), t()))
      case _ => None
    }
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, b) =>
      if(p(h)) cons(h, b)
      else b)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](p: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => p(a) append b)
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2())), (t1(), t2()))
      case _ => None
    }
  }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((f(Some(h()), None)), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((f(None, Some(h()))), (Empty, t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2()))), (t1(), t2()))
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_, _))

  def hasSubsequence[A](s: Stream[A]): Boolean = ???

  def startsWith[A](s2: Stream[A]): Boolean = {
    (this, s2) match {
      case (_, Empty) => true
      case (Cons(h1, t1), Cons(h2, t2)) if(h1() == h2()) => t1().startsWith(t2())
      case _ => false
    }
  }

  def startsWith2[A](s2: Stream[A]): Boolean = {
    this.zip(s2).forall(t => t._1 == t._2)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def tails: Stream[Stream[A]] = {
    (unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    }) append Stream(empty)
  }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (_.startsWith(s2))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = ???

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    Cons(() => h, () => t)
  }

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def ones: Stream[Int] = cons(1, ones)

  def ones2: Stream[Int] = unfold(1)(s => Some((1, 1)))

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(a, a))

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(n+1, n+1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a+b))
    }
    go(0, 1)
  }

  def fibs2(): Stream[Int] = unfold((0, 1)){case (a,b) => Some((a, (b,a+b)))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((h, s)) => cons(h, unfold(s)(f))
    }
  }

}

object StreamOps extends App {
  val s = Stream(1, 2, 3, 4, 5, 6, 7)

//  println(s.toListRecursive)
//  println(s.toList)
//  println(s.toListFast)

//  println(s.take(2).toList)
//  println(s.takeViaUnfold(2).toList)

//  println(s.takeWhile(x => x %2  != 0).toList)
//  println(s.takeWhile2(x => x %2  != 0).toList)

//  println(s.foldRight(1)(_ + _))

//  println(s.exists(x => x%2 == 0))
//  println(s.forall(x => x%2 == 0))

//  println(s.map(_ + 1).toList)
//  println(s.mapViaUnfold(_ + 1).toList)

//  println(s.filter(_ %2 != 0).toList)

//  println(s.append(Stream(8.0, 9.0)).toList)
//  println(s.flatMap(x => Stream(8.0, 9.0)).toList)

//  val ones: Stream[Int] = cons(1, ones)
//  println(ones.take(5).toList)
//  println(ones2.take(5).toList)

//  val threes: Stream[Int] = constant(3)
//  println(threes.take(5).toList)

//  val x = from(1)
//  println(x.take(5).toList)

//  println(fibs().take(5).toList)

//  println(s.zipWith(Stream(1, 2, 3, 4, 5, 6, 7))((a,b) => a+b).toList)
//  println(s.zip(Stream(1, 2, 3, 4, 5, 6, 7)).toList)
//  println(s.zipAll(Stream(1, 2, 3, 4)).toList)

//  println(s.startsWith(Stream(1, 2)))
//  println(s.startsWith(Stream(3, 4)))
//  println(s.startsWith2(Stream(1, 2)))
//  println(s.startsWith2(Stream(3, 4)))

  println(s.tails.map(_.toList).toList)
}
