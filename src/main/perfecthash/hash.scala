package main

import main.dptables.{choose, dp}

object hash {

  def hash_quinary(q: Int, length: Int, k: Int): Int = {
    var sum_numb = 0
    for (i <- 0 to length) {
      sum_numb = sum_numb + dp[q[i][length - i - 1][k]]
      k = k - q[i]
      if (k <= 0) {
        return sum_numb
      }
    }
    return sum_numb
  }

  def hash_binary(binary: Int, k: Int): Int = {
    val sum_numb = 0
    val length = 15
    for (i <- 0 to length) {
      if (binary & (1 << i)) {
        if (length - i - 1 >= k) {
          sum_numb = sum_numb + choose[length - i - 1][k]
        }
        k = k - 1
        if (k == 0) {
          return sum_numb
        }
      }
    }
    return sum_numb
  }

}
