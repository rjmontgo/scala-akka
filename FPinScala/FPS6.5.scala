type Rand[+A] = RNG => (A, RNG)

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG)

  def double: (Double, RNG)
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

  def doubleMap: (Double, RNG) = {
    SimpleRNG.map(_.nonNegativeInt)(int => (int.toDouble / Int.MaxValue))(this)
  }
}

object SimpleRNG {
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rngInt) = rng.nextInt
    val (double, rngDouble) = rngInt.double
    ((int, double), rngDouble)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (double, rngDouble) = rng.double
    val (int, rngInt) = rngDouble.nextInt
    ((double, int), rngInt)
  }

  def doubleThree(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rngDouble1) = rng.double;
    val (double2, rngDouble2) = rngDouble1.double;
    val (double3, rngDouble3) = rngDouble2.double;
    ((double1, double2, double3), rngDouble3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (int, nextRNG) = rng.nextInt
    if (count == 1)
      (int :: Nil, nextRNG)
    else {
      val (list, finalRng) = ints(count - 1)(nextRNG)
      (int :: list, finalRng)
    }
  }

  def unit[A](a: A): Rand[A] = 
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

}