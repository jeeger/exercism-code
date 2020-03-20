object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = 
    (1 until limit).filter(x => factors.exists(y => x % y == 0)).sum
}

