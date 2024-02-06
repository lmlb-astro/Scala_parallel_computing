package scalashop

import org.scalameter.*

object VerticalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")


/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface:

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // define the variables
    var col = from;

    while(col < end) {
      var row = 0
      while (row < src.height) {
        val blurred_pix = boxBlurKernel(src, col, row, radius)
        dst.update(col, row, blurred_pix)
        row = row + 1
      }
      col = col + 1
    }
  }


  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   *  TODO implement using the `task` construct and the `blur` method
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // Create the list of tuples with begin and end points
    var delta_col = src.width/numTasks
    if (delta_col < 1) {
      delta_col = 1
    }
    val splitting_points = 0 to src.width by delta_col
    val ends = splitting_points.tail
    val tuples_list = splitting_points.zip(ends)

    // Initiate the tasks
    val tasks = tuples_list.map(tup => task {blur(src, dst, tup._1, tup._2, radius)})
    // Wait for them to end
    tasks.foreach(_.join)
  }

