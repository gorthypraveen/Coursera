package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def setFirstNNumbers(N: Int) (elem : Int): Boolean = (elem > 0 && elem < N)
    def sa(elem:Int):Boolean = setFirstNNumbers(10)(elem)
    def sb(elem:Int):Boolean = setFirstNNumbers(20)(elem)
    def setRangeOfNumbers(low:Int, high:Int)(elem:Int):Boolean = (elem >=low && elem <=high)
    def set1To100(elem:Int):Boolean = setRangeOfNumbers(1, 100)(elem)
    def set1To50(elem:Int):Boolean = setRangeOfNumbers(1, 50)(elem)
    def set51To100(elem:Int):Boolean = setRangeOfNumbers(51, 100)(elem)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s2, 3), "Set does not contain 3")
      assert(contains(s3, 3), "Set Contains 3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersection contains common elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Inter 1")
      assert(!contains(s, 2), "Inter 2")
      val si = intersect(s3,s3)
      assert(!contains(s, 3), "Inter 3")
    }
  }
  
  test("test diff method") {
     new TestSets{
       assert(!contains(diff(sa,sb), 1), "Test diff 1")
       assert(!contains(diff(sb,sa), 1), "Test diff 2")
       assert(contains(diff(sb,sa), 11), "Test diff 3")
     }
  }
  test("test filter method") {
    new TestSets {
    	assert(contains(filter(sb,sa), 8), "Test filter 1")
    	assert(!contains(filter(sb,sa), 12), "Test filter 2")
    }
  }
  
  test("forall"){
    new TestSets{
      //println(FunSets.toString(sa))
      //println(FunSets.toString(sb))
      assert(forall(sa, sb), "Test whether all numbers in set of first 10 integers are in the range of first 20 integers")
      assert(!forall(sb, sa), "Test whether all numbers in set of first 20 integers are in the range of first 10 integers")
    }
  }
  
  test("exists")
  {
    new TestSets{
      assert(exists(set1To100, set1To50), "Set of first 100 numbers must have atleast one number in set of first 50 numbers")
      assert(!exists(set1To50, set51To100), "Set of first 50 numbers must NOT have any number between 51 and 100")
    }
  }
  
  test("map")
  {
    new TestSets{
    	val m = map(set1To50, x=>2*x)
    	println(FunSets.toString(m))
    	assert(contains(m, 15), "15*2 must be in set of first 50 integers")
    	assert(!contains(m,40), "40*2 must NOT be in the set of first 50 integers")
    }
    
  }
}
