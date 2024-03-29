package reductions

import org.scalameter.*

object LineOfSightRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 100,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

enum Tree(val maxPrevious: Float):
  case Node(left: Tree, right: Tree) extends Tree(left.maxPrevious.max(right.maxPrevious))
  case Leaf(from: Int, until: Int, override val maxPrevious: Float) extends Tree(maxPrevious)

object LineOfSight extends LineOfSightInterface:

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    // set line of sight at initial point of view
    var max_angle: Float = 0
    output(0) = max_angle

    // compute the angle for all points in the array
    var idx = 1
    while (idx < input.length) {
      // store line of sight angle
      val angle = input(idx)/idx
      output(idx) = angle
      if (angle < max_angle) {
        output(idx) = max_angle
      }

      // update max_angle
      if (max_angle < angle) {
        max_angle = angle
      }

      idx = idx + 1
    }
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var max_angle: Float = 0; var idx = from.max(1) // To avoid a division by 0

    while (idx < until) {
      val angle = input(idx)/idx
      if(angle > max_angle) {
        max_angle = angle
      }
      idx = idx + 1
    }

    max_angle
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Tree.Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Tree.Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if (end - from <= threshold) {
      // returns a leaf
      val max_angle = upsweepSequential(input, from, end)
      Tree.Leaf(from, end, max_angle)
    } else {
      //returns a node
      val mid = (end + from)/2
      val (node_l, node_r) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold))
      Tree.Node(node_l, node_r)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    var max_angle = startingAngle; var idx = from

    // case you start at zero (avoids dividing by zero)
    if (idx == 0) {
      output(idx) = max_angle
      idx = idx + 1
    }

    // run over the rest of the array
    while (idx < until) {
      // store the correct value in the output array
      val angle = input(idx)/idx
      output(idx) = angle
      if(max_angle > angle) {output(idx) = max_angle}

      // update the max_angle
      if(angle > max_angle) {max_angle = angle}

      idx = idx + 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    tree match{
      case Tree.Leaf(from, until, maxPrevious) => {downsweepSequential(input, output, startingAngle, from, until)}
      case Tree.Node(left, right) => {parallel(downsweep(input, output, startingAngle, left), downsweep(input, output, startingAngle.max(right.maxPrevious), right))}
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val startAngle: Float = 0
    val angle_tree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, startAngle, angle_tree)
  }
