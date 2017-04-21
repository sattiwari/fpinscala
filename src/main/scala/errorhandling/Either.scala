package errorhandling

// Q7
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if(xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Double, y: Double): Either[Exception, Double] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
  }

//  Q8
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a.foldRight[Either[E, List[A]]](Right(Nil))((elem, acc) => elem.map2(acc)(_ :: _))
  }

  def traverse[E, A](a: List[A])(f: A => Either[E, A]): Either[E, List[A]] = {
    a.foldRight[Either[E, List[A]]](Right(Nil))((elem, acc) => f(elem).map2(acc)(_ :: _))
  }

//Q9
  /*
  There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
  approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

  trait Partial[+A,+B]
  case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
  case class Success[+B](get: B) extends Partial[Nothing,B]

  There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
  `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
  accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
  values into a list; we can accumulate values using any user-supplied binary function.

  It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
  helper functions like `map2` and `sequence`.
  */

}
