/**
 * object returns pseudorandom numbers in specific order
 */
object Random {
  private var count = 0
  private val pseudoRand: Array[Int] = Array(8, 346, 1, -74, 50, 33, -91, -1, 1, 74, 55, -23, -14)

  def randInt: Int = {
    count += 1
    if (count == pseudoRand.length) count = 1
    pseudoRand(count - 1)
  }
}
