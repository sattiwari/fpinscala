package functions

object PureFunctions extends App {
  val x = "Hello, world"
  var r1 = x.reverse
  var r2 = x.reverse
  println(r1)
  println(r2)

  //  reverse is purely functional & referentially transparent
  r1 = "Hello, world".reverse
  r2 = "Hello, world".reverse
  println(r1)
  println(r2)

  //  append is not purely functional and referentially transparent
  val y = new StringBuilder("Hello")
  val s1 = y.append(", world")
  println(s1)
  val s2 = y.append(", world")
  println(s1)
  println(s2)
}