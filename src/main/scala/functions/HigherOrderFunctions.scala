package functions

/**
  * Created by satendra on 04/08/16.
  */

trait PrintUtils {
  def printResult(str: String, n:Int, f: Int => Int) {
    println(s"${n}th $str term is ${f(n)}")
  }
}

//ch01 - Q1
object HigherOrderFunctionsDemo extends App with PrintUtils{
  def factorial(x: Int) = {
    def loop(x:Int, acc: Int): Int = {
      if(x == 1) acc
      else loop(x-1, x * acc)
    }
    loop(x, 1)
  }

  def fibonacci(n: Int): Int = {
    def loop(n: Int, a: Int, b: Int): Int = {
      if(n==0) b
      else loop(n-1, b, a+b)
    }
    loop(n, 0, 1)
  }

  printResult("factorial", 5, factorial)
  printResult("fibonacci", 5, fibonacci)
}

object AnonymousFunctionsDemo extends App with PrintUtils{
  printResult("increment", 1, x => x+1)
}

// ch01 - Q3
object PartialFunctionsDemo extends App {
  def partial1[A, B, C](a: A, f: (A,B) => C): B => C = {
    b:B => f(a,b)
  }

  val pf1 = partial1(1, (x: Int, y: String) => s"$x, $y")
  println(pf1("abc"))
}

// ch01 - Q4, Q5
object CurryingDemo extends App {
  def curry[A, B, C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A, B, C](f: A => B => C): (A,B) => C = {
    (a: A, b:B) => f(a)(b)
  }
}

// ch01 - Q6
object CompositionDemo extends App {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}