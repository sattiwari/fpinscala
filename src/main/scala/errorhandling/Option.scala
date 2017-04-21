package errorhandling

// hide std lib Option & Either, since we are writing our own
import scala.{Either => _, Option => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) this else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option1 {
  private def mean(xs: Seq[Double]): scala.Option[Double] = {
    if(xs.nonEmpty) scala.Some(xs.sum / xs.length)
    else scala.None
  }

//  Q2
  def variance(xs: Seq[Double]): scala.Option[Double] = mean(xs) flatMap {m => mean(xs.map(x => math.pow(x - m, 2)))}

//  Q3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a.flatMap(ai => b.map(bi => f(ai,bi)))

//  Q4
  import errorhandling.Pattern1._
  def bothMatch_2(pat: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat), mkMatcher(pat2))((f, g) => f(s) && g(s))
  }

//  Q5
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hi => sequence(t) map(hi :: _))
    }
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((elem, acc) => map2(elem, acc)(_ :: _))
  }

//  Q6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((elem, acc) => map2(f(elem), acc)(_ :: _))
  }

  
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}

object OptionOps extends App {

  var x = Option(4)
  var x2 = Option(Option(4))
  var y = None

//  println(x.map(t => t+1))
//  println(x.getOrElse(1))
//  println(y.getOrElse(1))
//    println(x.orElse(Option(1)))
//  println(y.orElse(None))
//  println(x.flatMap(t => Option(t+1)))
//  println(x.filter(t => t %2 == 0))
//  println(x.filter(t => t %2 != 0))

  import Option1._
  import errorhandling._
//  println(sequence_1[Int](List(Option(1), Option(3), Option(2))))



}