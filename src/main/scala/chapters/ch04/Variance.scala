package chapters.ch04

object Variance {
  def variance(xs: Seq[Double]): Option[Double] = { // math.pow(x - m, 2)

    def mean(_xs: Seq[Double]): Double = _xs.sum / _xs.size

    if(xs.isEmpty) None
    else {
      val meanXs = mean(xs)

      val temp = xs.map(v => Math.pow(v - meanXs, 2))

      Some(mean(temp))
    }
  }
}
