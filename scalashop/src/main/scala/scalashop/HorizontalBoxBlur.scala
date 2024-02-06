package scalashop

import org.scalameter.*

object HorizontalBoxBlurRunner:

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface:

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var row = from

    // Loop over the row interval and blur each pixel
    while (row < end) {
      var col = 0
      while (col < src.width) {
        val blurred_pix = boxBlurKernel(src, col, row, radius)
        dst.update(col, row, blurred_pix)
        col = col + 1
      }
      row = row + 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // Create the list of tuples with the start and end points for each task
    var delta_row = src.height/numTasks
    if(delta_row < 1) {
      delta_row = 1
    }

    val strip_points = 0 to src.height by delta_row
    val end_points = strip_points.tail
    val tuples_list = strip_points.zip(end_points)

    // perform the task for each tuple
    val tasks = tuples_list.map(tup => task {blur(src, dst, tup._1, tup._2, radius)})
    // wait for all tasks to complete
    tasks.foreach(_.join)
  }

