package reductions

import org.scalameter.*

object ParallelCountChangeRunner:

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns := 20,
    Key.exec.maxWarmupRuns := 40,
    Key.exec.benchRuns := 80,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val amount = 250 //6 //250
    val coins = List(1, 2, 5, 10, 20, 50) //List(1, 2, 3) //List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    catch
      case e: NotImplementedError =>
        println("Not implemented.")

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))

object ParallelCountChange extends ParallelCountChangeInterface:

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def perform_count(money: Int, coins: List[Int], count: Int): Int = {
    var count_temp = count
    if (money > 0 && coins.length > 0) {
      count_temp = perform_count(money - coins.head, coins, count_temp)
      count_temp = perform_count(money, coins.tail, count_temp)
    } else if (money == 0) {
      count_temp = count_temp + 1
    }
    count_temp
   }

  def countChange(money: Int, coins: List[Int]): Int = {
    var count = 0
    perform_count(money, coins, count)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    var count_temp = 0
    if(threshold(money, coins)) {
      count_temp = countChange(money, coins)
    } else if (money > 0 && coins.length > 0) {
      val (count1, count2) = parallel(parCountChange(money - coins.head, coins, threshold), parCountChange(money, coins.tail, threshold))
      count_temp = count1 + count2
    } else if (money == 0) {
      count_temp = 1
    }
    count_temp
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    (money: Int, coins: List[Int]) => if (money <= 2*startingMoney/3) {true} else {false}
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    (money: Int, coins: List[Int]) => if (coins.length <= 2*totalCoins/3) {true} else {false}
  }


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (money: Int, coins: List[Int]) => if (money*coins.length <= startingMoney*allCoins.length/2) {true} else {false}
  }
