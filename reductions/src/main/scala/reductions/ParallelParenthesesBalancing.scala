package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var idx = 0; var count_left = 0

    while (idx < chars.length){
      if (chars(idx) == ')' && count_left == 0) {
        return false
      } else if (chars(idx) == ')') {
        count_left = count_left - 1
      } else if (chars(idx) == '(') {
        count_left = count_left + 1
      }
      idx = idx + 1
    }

    if (count_left == 0) {
      return true
    }
    false
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int, Boolean) = {
      var i = idx; var left_count = 0; var right_count = 0
      var balance_ver = true
      while (i < until) {
        if(chars(i) == ')' && left_count == right_count) {
          balance_ver = false
          right_count = right_count + 1
        } else if (chars(i) == ')') {
          right_count = right_count + 1
        } else if(chars(i) == '(') {
          left_count = left_count + 1
        }
        i = i + 1
      }
      (left_count, right_count, balance_ver)
    }

    def reduce(from: Int, until: Int): (Int, Int, Boolean) = {
      if (until - from < threshold) {
        // perform and return the sequential calculation
        traverse(from, until, 0, 0)
      } else {
        // perform the parallel calculation
        val mid = (from + until)/2
        val ((left1, right1, ver1), (left2, right2, ver2)) = parallel(reduce(from, mid), reduce(mid, until))

        // calculate the new left and right
        val new_left = left1 + left2
        val new_right = right1 + right2

        // update the balance boolean
        var new_bool = ver1
        if(new_right > new_left) {
          new_bool = false
        }

        // return the result
        (new_left, new_right, new_bool)
      }
    }

    val (end_left, end_right, end_bool) = reduce(0, chars.length)
    var return_bool = false
    if (end_bool && end_left == end_right) {return_bool = true}
    return_bool
  }

  /* def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      ???
    }

    reduce(0, chars.length) == ???
   */

  // For those who want more:
  // Prove that your reduction operator is associative!

