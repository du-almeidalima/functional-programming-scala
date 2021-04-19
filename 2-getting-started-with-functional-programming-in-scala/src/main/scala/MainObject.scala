import scala.annotation.tailrec
import scala.collection.immutable.IntMap.Nil.andThen

object MainObject {
  def factorial(num: Int): Int = {
    // In Scala it's possible to define a function inside another function and use it as a helper.
    // In here, I'm using it to do a loop, those functions are usually called "go" or "loop"
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, acc * n)
    }

    go(num, 1)
  }

  def fibonacci(num: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int, cur: Int): Int = {
      if (n == 0) acc
      else go(n - 1, acc + cur, acc)
    }

    go(num, 0, 1)
  }

  def abs(num: Int): Int = {
      if (num < 0) -num
      else num
  }

  // 2.4 HOF (High Order Functions)
  def formatResult(name: String, num: Int, fn: Int => Int): String = {
    s"The $name of $num is ${fn(num)}"
  }

  // 2.5 Polymorphic function to find an element in a array
  def findItem[T](arr: Array[T], assFn: T => Boolean): Option[T] = {
    @tailrec
    def loop(i: Int): Option[T] = {
      if (assFn(arr(i))) return Some(arr(i))
      else if (i >= arr.length) return None
      loop(i + 1)
    }

    loop(0)
  }

  // 2.6 Partial Application
  def createPartialCounter[A](a: A, counterFn: (A, A) => A): A => A = {
    (num: A) => counterFn(a, num)
  }

  // 2.6 Composition
  def createUselessFunction(): Int => Double = {
    val twice = (a: Int) => a * 2
    val half = (a: Int) => a / 2d

    twice.andThen(half) ;
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def main(args: Array[String]): Unit = {
    println(formatResult("Absolute", -42, this.abs))
    println(formatResult("Factorial", 3, this.factorial))
    println(formatResult("Fibonacci", 5, this.fibonacci))

    println("Find 3 in Array [1, 2, 3, 4, 5, 6]: ", findItem(Array(1, 2, 3, 4, 5), (x: Int) => x == 3))

    val counter = this.createPartialCounter[Int](5, (a, b) => a + b)
    val uselessFn = this.createUselessFunction()

    println("Partial Counter: " + counter(0))
    println("Function that Doubles and Divide a number" + uselessFn(10))
  }
}
