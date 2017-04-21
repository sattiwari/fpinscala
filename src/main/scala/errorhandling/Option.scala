package errorhandling

// hide std lib Option & Either, since we are writing our own
import scala.{Option => _, Either => _,_}

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
    if(xs.nonEmpty) Some(xs.sum / xs.length)
    else scala.None
  }

  def variance(xs: Seq[Double]): scala.Option[Double] = mean(xs) flatMap {m => mean(xs.map(x => math.pow(x - m, 2)))}

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
  println(x.filter(t => t %2 == 0))
  println(x.filter(t => t %2 != 0))


}