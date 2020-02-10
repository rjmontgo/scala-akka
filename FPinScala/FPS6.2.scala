trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt: (Int, RNG) = {
    val (n, nextRNG) = nextInt
    (Math.abs(n + 1), nextRNG)
  }

  def double: (Double, RNG) = {
    val (int, nextRNG) = nonNegativeInt
    (int.toDouble / Int.MaxValue, nextRNG)
  }
}