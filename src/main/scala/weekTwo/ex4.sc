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


  /**
    * Expressive power of a lagnauge is increased if we can functions
    * as arguments
    */

  def avarageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
  def sqrt(x: Double) =
    fixedPoint(avarageDamp(y => x / y))(1)

  /**
    * Avarage Dump takes one function as argument and return another function
    *
    */

  sqrt(2)
}