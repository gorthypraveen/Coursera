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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
  {
    if(c<0||r<0)-1
    else if(c==0)1
    else if(c==r)1
    else pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
  {
    def balanceInternal(chars:List[Char], open:Int, closed:Int):Boolean =
    {
		if(chars.isEmpty) (open==closed)
		else
		{
		   val n = chars.head;
		   val newOpen = open + (if(n == '(') 1 else 0)
		   val newClosed = closed + (if(n==')')1 else 0 )
		   if(newOpen >= newClosed) balanceInternal(chars.tail, newOpen, newClosed)
		   else false
		}
    }
    balanceInternal(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
  {
     if(money <= 0) 0
     else if(coins.isEmpty) 0
     else
     {	/**
      	*   Say n is money and {d1,d2,d3} are denominatins
      	*   The number of ways would be sum
      	*   	1. Ignore d1 and see number of ways you can get change for n with {d2,d3}
      	*       2. Number of ways you can get a change for (n-d1) with {d1,d2,d3}
      	*       
      	*/
        countChange(money, coins.tail) + 
        {
           if(money==coins.head) 1
           else countChange(money-coins.head, coins)
        }
     }
       
  }
}
