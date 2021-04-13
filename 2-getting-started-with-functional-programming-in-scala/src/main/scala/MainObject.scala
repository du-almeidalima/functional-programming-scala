import scala.annotation.tailrec

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

  def formatResult(name: String, num: Int, fn: Int => Int): String = {
    s"The $name of $num is ${fn(num)}"
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("Absolute", -42, this.abs))
    println(formatResult("Factorial", 3, this.factorial))
    println(formatResult("Fibonacci", 5, this.fibonacci))
  }
}
