package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c==0) || (r==c)) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char], openb: Int): Boolean = {
      if (chars.length == 0) (openb==0)
      else
      {  if (chars(0) != '(' && chars(0) != ')') balance2(chars.slice(1,chars.length), openb)
        else  {if (chars(0) == '(') balance2(chars.slice(1,chars.length), openb+1) else (openb>0) && balance2(chars.slice(1,chars.length), openb-1)}
      }
    }
    if (chars.length == 0) true
    else balance2(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.length ==0 ) {if (money == 0) 1 else 0}
    else {countChange(money, coins.slice(0,coins.length-1)) + countChange(money - coins(coins.length-1), coins)}
  }
}
