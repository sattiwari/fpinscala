package functions

object PolymorphicFunctionsDemo extends App {

  def binarySearch[A](arr: Array[A], key: A, gt: (A, A) => Boolean): Boolean = {
    def loop(low: Int, high: Int): Boolean = {
      if(low > high) false
      else {
        val mid = (low + high) / 2
        if(arr(mid) == key) true
        else {
          if(gt(arr(mid), key)) loop(0, mid-1)
          else loop(mid+1, high)
        }
      }
    }
    loop(0, arr.length)
  }
  println(binarySearch(Array(1,2,3,4,5), 5, (x: Int, y: Int) => x > y))
  println(binarySearch(Array(1.0,2.0,3.0,4.0,5.0), 1.0, (x: Double, y: Double) => x > y))
  println(binarySearch(Array("a", "b", "c", "d", "e"), "a", (x: String, y: String) => x > y))

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if(n==0) true
      else {
        if(gt(as(n), as(n-1))) loop(n-1)
        else false
      }
    }
    loop(as.length-1)
  }
  println(isSorted(Array(1,2,3,4,5), (x: Int, y: Int) => x > y))
}