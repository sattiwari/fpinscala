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

//  ch03 - Q2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

//  ch03 - Q3
  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n == 0) l
    else drop(n - 1, tail(l))
  }

//  ch03 - Q4
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }
  }

//  ch03 - Q5
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

//  ch03 - Q6
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

//  ch03 - Q9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)


//  ch03 - Q10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

//  ch03 - Q11
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3(l: List[Int]) = foldLeft(l, 1)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  //  TODO why replacing h with underscore doesn't work
//  ch03 - Q12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

//  ch03 - Q13
  //    def foldLeft2[A, B](l: List[A], z:B)(f:(B,A) => B): B = foldRight(l, z)()
  //  def foldRight2[A, B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(l, z)(f)

//  ch03 - Q14
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((elem, acc) => Cons(elem, acc))

//  ch03 - Q15
  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]()) { (elem, acc) =>
      foldRight(elem, acc)((ielem, iacc) => Cons(ielem, iacc))
    }
  }

//  ch03 - Q18
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((elem, acc) => Cons(f(elem), acc))

//  ch03 - Q19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)

//  ch03 - Q20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((elem, acc) => append(f(elem), acc))

//  Q21
  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if(f(x)) List(x) else Nil)

//  Q22
  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairWise(t1, t2))
    }
  }

//  Q23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f:(A,B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }
  }

//  Q24
  def hasSubSequence[A](l: List[A], sub: List[A]): Boolean = ???

}

object ListDemo extends App {
  import List._

//  val example = Cons(1, Cons(2, Cons(3, Nil)))
//  val example2 = List(2,4,6,7,8,9)
//  println(List.zipWith(example, example)(_ * _))
//
//  // ch03 - Q8
//  val x = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
//  println(x)

//  println(append2(List(1, 2, 3), List(4, 5, 6)))
//  println(append(List(1, 2, 3), List(4, 5, 6)))

//  println(concatenate(List(List(1, 6), List(2, 5), List(3, 4))))

//  println(map(List(1, 2, 3))(x => x + 1))
//  println(map(List(1, 2, 3))(x => x.toString))

//  println(filter(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0))
//  println(filterUsingFlatMap(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0))


}