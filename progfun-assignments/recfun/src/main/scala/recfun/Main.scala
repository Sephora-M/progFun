package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("(def (tf L) (cond ((null? L) 0) ((= (quote (car L)) 'true) (cons 1 (tf (cdr L)))) (else (cons (quote (car L)) (tf (cdr L))))) (tf (quote (1 2 true))))".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    // a recursive function that calculates x!
    def factorial(x: Int): Int ={
      //if (x == 0) 1 else x * factorial(x - 1)
      def loop(acc: Int, x: Int): Int =
        if (x==0) acc
        else loop(acc*x, x-1)
      loop(1,x);
    }
    def choose(n: Int, k: Int): Int = factorial(n) / (factorial(k) * factorial(n - k))
    if (c <0 || r <0) throw new IllegalArgumentException("please enter only non-negative integers")
    else choose(r, c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    // this function goes through chars and checks if every opening parentheses is followed by a closing one
    def matching(open: Int, chars: List[Char]): Boolean = {

      if (open < 0) false
      else if (chars.isEmpty) (open == 0)

      else if (chars.head == '(') matching(open + 1, chars.tail)
      else if (chars.head == ')') matching(open - 1, chars.tail)

      else matching(open, chars.tail)

    }
    matching(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // the degenerate cases
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else if (money < 0) 0
    /* the total is the sum of the ways there are to make change 
     * for the amount of money with all the denomination of coins-1 plus the 
     * number of ways there are to make change of the amount of money - the coins we 
     * left aside with the total denomitation of coins*/
    else (countChange(money, coins.tail) + countChange(money - coins.head, coins))
  }
}
