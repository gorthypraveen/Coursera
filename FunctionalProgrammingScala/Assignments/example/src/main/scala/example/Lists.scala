package example

import common._

object Lists {
  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def remove(num: Int, list: List[Int]) = list diff List(num)
  
  def sum(xs: List[Int]): Int = {
    def sumInternal(xs: List[Int], sum: Int, index:Int): Int =
      if(xs.isEmpty) sum
      else
        {
    	  val first = xs.head
    	  val newIndex = index+1
    	  val list = remove(index, xs)
    	  val newSum = sum + first
    	  sumInternal(list, newSum, newIndex)
        }
    sumInternal(xs, 0, 1)
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    def maxInternal(xs: List[Int], max: Int, index:Int): Int =
      if(xs.isEmpty) max
      else
      {
        val newMax = xs.head
    	  val newIndex = index+1
    	  val list = remove(index, xs)
        if(max > newMax) maxInternal(list, max, newIndex) else maxInternal(list, newMax, newIndex)
      }
    if(xs.isEmpty) 0
    else
    maxInternal(xs, Int.MinValue, 1)
  }
}
