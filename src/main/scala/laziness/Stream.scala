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

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

//  using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] = {
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

}

object StreamOps extends App {
  val s = Stream(1, 2, 3, 4, 5, 6, 7)

//  println(s.toListRecursive)
//  println(s.toList)
//  println(s.toListFast)

//  println(s.take(2).toList)

//  println(s.takeWhile(x => x %2  != 0).toList)
//  println(s.takeWhile2(x => x %2  != 0).toList)

//  println(s.foldRight(1)(_ + _))

//  println(s.exists(x => x%2 == 0))
//  println(s.forall(x => x%2 == 0))

//  println(s.map(_ + 1).toList)
//  println(s.filter(_ %2 != 0).toList)
//  println(s.append(Stream(8.0, 9.0)).toList)
//  println(s.flatMap(x => Stream(8.0, 9.0)).toList)
}
