package parallelism

import java.util.concurrent.ExecutorService

import scala.concurrent.Future

trait Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A]

  def map2[A, B, C](a: Par[A], b: Par[B], c: Par[C])(f: (A,B) => C): Par[C]

  def fork[A](a: => Par[A]): Par[A]

  def async[A](a: => A): Par[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}

object Examples {
  import Par._

  def sum(as: IndexedSeq[Int]): Int = {
    if(as.size <= 1) as.headOption getOrElse 0
    else {
      val (l, r) = as.splitAt(as.size / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.map2(sum(l), sum(r))(_ + _)
      Par.get(sumL) + Par.get(sumR)
    }
  }
}
