package recfun

import scala.annotation.tailrec

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
    */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (x, y) if x == y => 1
    case (_, _) => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceIter(openAcc: Int, chars: List[Char]): Boolean = {
      findParentheses(0, chars) match {
        case (_, -1) if openAcc == 0 => true
        case (_, -1) if openAcc > 0 => false
        case (true, i) if openAcc == 0 => balanceIter(openAcc + 1, takeRightAfterIdx(chars, i))
        case (true, i) if openAcc > 0 => balanceIter(openAcc + 1, takeRightAfterIdx(chars, i))
        case (false, _) if openAcc == 0 => false
        case (false, i) if openAcc > 0 => balanceIter(openAcc - 1, takeRightAfterIdx(chars, i))
      }
    }

    def takeRightAfterIdx(chars: List[Char], i: Int) = chars.takeRight(chars.length - 1 - i)

    @tailrec
    def findParentheses(i: Int, chars: List[Char]): (Boolean, Int) = {
      if (i == chars.length) return (false, -1) //means no parentheses
      chars(i) match {
        case '(' => (true, i)
        case ')' => (false, i)
        case _ => findParentheses(i + 1, chars)
      }
    }

    if (chars == null || chars.isEmpty) true else balanceIter(0, chars)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    (money, coins) match {
      case (0, _) => 1
      case (x, xs) if  xs == null || xs.isEmpty || x < 0 => 0
      case (_, _) => countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
