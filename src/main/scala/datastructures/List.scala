package datastructures

/**
  * Created by satendra on 05/08/16.
  */
//TODO implement following operations for Vector
sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(l: List[Int]): Int = {
    l match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(l: List[Int]): Int = {
    l match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def fill[A](n: Int, a: A): List[A] = ???

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n == 0) l
    else drop(n - 1, tail(l))
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }
  }

  def setHead[A](l: List[A], x: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(x, t)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(last, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(l: List[Int]) = foldRight(l, 0)(_ + _)

  def product2(l: List[Int]) = foldRight(l, 1)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)


  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]) = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  //  TODO why replacing h with underscore doesn't work
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  //    def foldLeft2[A, B](l: List[A], z:B)(f:(B,A) => B): B = foldRight(l, z)()
  //  def foldRight2[A, B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(l, z)(f)

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a2, a1)((acc, elem) => Cons(elem, acc))

  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]()) { (elem, acc) =>
      foldRight(elem, acc)((ielem, iacc) => Cons(ielem, iacc))
    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((elem, acc) => Cons(f(elem), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((elem, acc) => append(f(elem), acc))

  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if(f(x)) List(x) else Nil)

  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairWise(t1, t2))
    }
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f:(A,B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }
  }

  def addCorrespondingElements[A](l1: List[A], l2: List[A]): List[A] = ???

  def hasSubSequence[A](l: List[A], sub: List[A]): Boolean = ???

}

object ListDemo extends App {
  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(2,4,6,7,8,9)
  println(List.zipWith(example, example)(_ * _))


}