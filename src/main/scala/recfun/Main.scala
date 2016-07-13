package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    * 1
    * 1 1
    * 1 2 1
    * 1 3 3 1
    * 1 4 6 4 1
    * ...
    *
    * This is the pascal triangle
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == c || c == 0) 1 else (pascal(c, r - 1) + pascal(c - 1, r - 1))
  }

  /**
    * Exercise 2
    * chars.isEmpty: Boolean returns whether a list is empty
    * chars.head: Char returns the first element of the list
    * chars.tail: List[Char] returns the list without the first element
    * ####TESTING#####
    * You can use the toList method to convert from a string to aList[Char]: e.g "(just an)example".toList
    */
  def balance(chars: List[Char]): Boolean = {

    def check(chars: List[Char], n: Int, flag: Boolean): Int = {
      if (chars.isEmpty) {
        if (flag == true) n else -3
      }
      else {
        if (chars.head == ')') check(chars.tail, 1, true) + n
        else if (chars.head == '(') check(chars.tail, -1, false) + n else check(chars.tail, n, flag)
      }
    }

    if (check(chars, 0, false) == 0) true
    else false
  }

  /**
    * Exercise 3
    * Write a recursive function that counts how many different ways you can make change for an amount,
    * given a list of coin denominations. For example, there are 3 ways to give change for
    * 4 if you have coins (1,2): 1+1+1+1, 1+1+2, 2+2
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
