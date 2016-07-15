object exercises2 {

  /**
   * Tail recursion of sum
   * Higher Order Functions
   * Functional lanaguages treat functions as first-class values
   * This means a function can be passed as a parameter and returned
   * as result
   * -> More Flexible
   *


   * Function Types
   * A => B is the type of a function that takes an argument
   * of a type A and return type B
   * (f: Int => Int) is the map integers to integers


    Anonymous Functions
    Passing functions as parameters leads to the creation of many
    small functions

    def str = "abc"; println(str)
    we can directly write
    println("abc")

    Example:
    (x: Int) => x * x * x
    (x: Int) is the parameter of the function and x*x*x is the body

    def f(xi : Ti, ...Xn: Tn) = E;f]
    where f is a fresh name

     def sumInts(a: Int, b: Int) = sum(x => x, a, b)
     def sumCubes(a: Int, b: Int) = sum(x => x*x*x, a, b)
   */



  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a , 0)
  }

  sum(x => x * x, 3, 5)

}/**CLOSE OBJECT*/