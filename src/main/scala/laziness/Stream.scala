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
        case _ => Nil
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

  def take(n: Int): Stream[A] = {
    @tailrec
    def go(n: Int, s: Stream[A], acc: Stream[A]): Stream[A] = {
      (n, s, acc) match {
        case (0, _, _) => acc

        case (_, Cons(h, t), _) =>
          go(n-1, t(), Cons(h, () => acc))
      }
    }

    go(n, this, Empty)
  }

  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if (p(h)) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
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