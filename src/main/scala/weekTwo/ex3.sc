object ex3 {
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a+1, b)
    sumF /* Value that gets returned (a Function!) */
  } /** sum is now a function that returns another function (sumF)*/

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  /*def sumFactorials = sum(fact)*/

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

  def product(t: Int => Int)(a: Int, b: Int) : Int = mapReduce(t, (x, y) => x * y, 1)(a,b)
  product(x => x * x)(3,4)

  def fact(n: Int) = product(x => x)(1, n)

  fact(5)

  /**
    *  Currying
    * sumInt(a: Int, b: Int) = sum(x => x, a, b)
    * sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
    * sumFact(a: Int, b: Int) = sum(fact, a, b)
    * a and b get passed unchanged
    */


  /**
    * sum(cube)(1,10)
    * sum(cube) applies sum to cube and returns the sum of cubes function,
    * sum(cube) is therefore a equivalent to sumCubes
    * this function is next applied to the arguments (1, 10)
    * Generally, function application associates to the left:
    * sum(cube)(1, 10) == (sum (cube)) (1, 10)
    * (sum(cube)) returns a functions and that function apples (1,10)
    */

  /**  Multi Parameter Lists
    *  def sum(f: Int => Int)(a: Int, b: Int): Int =
    *   if (a > b) 0 else f(a) + sum(f)(a + 1, b)
    */

}