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


  /**
    * | Denote an alternative, [...] an option (0 or 1)
    * {...} a repetition (0 or more)
    *
    * Type         = SimpleType | FunctionType
    * FunctionType = SimpleType '=>' Type
    *               | '(' [Types] ')' '=>' Type
    * SimpleType   = Ident
    * Types        = Type { '.' Type}
    *
    * A Type can be Int, Double, Byte, Short, Char, Long, Float
    *
    *
    * Expressions can be:
    *  -> An identifier such as x, isGoodEnough,
    *  -> A literal, like 0,1.0, "abc"
    *  -> A function application, like sqrt(x)
    *  -> An operator application, like -x, y + x,
    *  -> A selection, like math.abs
    *  -> A conditional expression, like if (x < 0) -x else x
    *  -> A block, like { val x = math.abs(y) ; x * 2 }
    *  -> An anonymous function, like x => x + 1
    *
    * Definition
    *   Def         = FunDef | ValDef
    *   FunDef      = def ident {'{' [Parameters] '}'}
    *                 [':' Type] '=' Expr
    *   ValDef      = val ident [':' Type] '=' Expr
    *   Parameter   = ident ':' [ '=>' ] Type
    *   Parameters  = Parameter { ',' Parameter }
    *
    *  A definition can be:
    *   A function definition, like def square(x: Int) = x * x
    *   A value definition, like val y = square(2)
    *
    *  A Parameter can be:
    *   A call-by-value parameter, like (x: Int),
    *   A call-by-name parammeter, like (y: => Double).
    *
    */
}