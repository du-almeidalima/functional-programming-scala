object MainObject {
  def abs(num: Int): Int = {
      if (num < 0) -num
      else num
  }

  def formatAbs(num: Int): String = {
    s"Absolute number of $num is ${this.abs(num)}"
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }
}
