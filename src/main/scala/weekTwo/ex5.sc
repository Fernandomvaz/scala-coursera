object ex5 {

  /*def addRational(r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom)

  def makeString(r: Rational) =
    r.numer + "/" + r.denom

  makeString(addRational(new Rational(1, 2), new Rational(2, 3)))*/

  val y = new Rational(5, 7)
  val x = new Rational(1, 3)
  val z = new Rational(3, 2)
  x.numer
  x.denom
  x.sub(y).sub(z)

  class Rational(x: Int, y: Int) {
    def numer = x
    def denom = y

    def add(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    /*
    My Solutions:
    def neg(that: Rational) =
      new Rational(that.numer * -1, that.denom * -1)

    def sub(that: Rational) =
      new Rational(
        numer * that.denom - that.numer * denom,
        denom * that.denom)*/

    def neg: Rational = new Rational(-numer, denom)

    def sub(that: Rational) = add(that.neg)

    override def toString = numer + "/" + denom
  }

}
/**
  * Rational Addition
  */

/**
  * A new type, named Rational
  * A constructor Rational to create elements of this type
  *
  * Scala keeps the name of types and values in different namespaces.
  * So there's no conflict between the two definitions of Rational
  *
  * We call the elements of a class type objects
  * We create and object by prefixing an application of the constructor of the class
  * with the operator new
  */

/**
  * Methods
  *
  * One can go further and also package functions operating on a data
  * abstraction in the data abstraction itself
  *
  * Such functions are called methods
  *
  * Example:
  * Rational numbers now would have, in addition to the functions numer and denom,
  * the functions add, sub, mul, div, equal, toString
  */

