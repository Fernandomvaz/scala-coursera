object ex4 {
  /**
    *   Finding a fixed point of a function
    *
    * A number X is called a fixed point of a function f if
    * f(x) = x
    *
    * x, f(x), f(f(x)), f(f(f(x))),...
    */
  def abs(x: => Double) = if (x < 0) -x else x

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess) /*CALLING*/
  }
  fixedPoint(x => 1 + x/2)(1)
}