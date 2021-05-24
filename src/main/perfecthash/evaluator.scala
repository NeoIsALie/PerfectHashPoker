package main

import main.dptables.suits
import main.hash.{hash_quinary, hash_binary}

class evaluator {
    var rank_map = Map(
      '2' -> 0,
      '3' -> 1,
      '4' -> 2,
      '5' -> 3,
      '6' -> 4,
      '7' -> 5,
      '8' -> 6,
      '9' -> 7,
      'T' -> 8,
      'J' -> 9,
      'Q' -> 10,
      'K' -> 11,
      'A' -> 12
    )

    var suit_map = Map(
      'C' -> 0,
      'D' -> 1,
      'H' -> 2,
      'S' -> 3,
      'c' -> 0,
      'd' -> 1,
      'h' -> 2,
      's' -> 3
    )

    var binaries_by_id = Array(
      0x1, 0x1, 0x1, 0x1,
      0x2, 0x2, 0x2, 0x2,
      0x4, 0x4, 0x4, 0x4,
      0x8, 0x8, 0x8, 0x8,
      0x10, 0x10, 0x10, 0x10,
      0x20, 0x20, 0x20, 0x20,
      0x40, 0x40, 0x40, 0x40,
      0x80, 0x80, 0x80, 0x80,
      0x100, 0x100, 0x100, 0x100,
      0x200, 0x200, 0x200, 0x200,
      0x400, 0x400, 0x400, 0x400,
      0x800, 0x800, 0x800, 0x800,
      0x1000, 0x1000, 0x1000, 0x1000,
    )

  var suitbit_by_id: Any = Array.fill[Byte](13)(0x1, 0x8, 0x40, 0x200)

  def evaluate_5cards(a: Int, b: Int, c: Int, d: Int, e: Int) {
    var suit_hash = 0

    suit_hash = suit_hash + suitbit_by_id[a]
    suit_hash = suit_hash + suitbit_by_id[b]
    suit_hash = suit_hash + suitbit_by_id[c]
    suit_hash = suit_hash + suitbit_by_id[d]
    suit_hash = suit_hash + suitbit_by_id[e]

    if (suits[suit_hash]) {
      val suit_binary = Array(0, 0, 0, 0)

      suit_binary[a & 0x3] = suit_binary[a & 0x3] | binaries_by_id[a]
      suit_binary[b & 0x3] = suit_binary[b & 0x3] | binaries_by_id[b]
      suit_binary[c & 0x3] = suit_binary[c & 0x3] | binaries_by_id[c]
      suit_binary[d & 0x3] = suit_binary[d & 0x3] | binaries_by_id[d]
      suit_binary[e & 0x3] = suit_binary[e & 0x3] | binaries_by_id[e]

      return flush[suit_binary[suits[suit_hash] - 1]]
    }

    var quinary = Array.fill[Int](13)(0)

    quinary[a >> 2] = quinary[a >> 2] + 1
    quinary[b >> 2] = quinary[b >> 2] + 1
    quinary[c >> 2] = quinary[c >> 2] + 1
    quinary[d >> 2] = quinary[d >> 2] + 1
    quinary[e >> 2] = quinary[e >> 2] + 1

    return noflush5[hash_quinary(quinary, 13, 5)]
  }

  def evaluate_7cards(a:Int, b:Int, c:Int, d:Int, e:Int, f:Int, g:Int) {
    val suit_hash = 0

    suit_hash = suit_hash + suitbit_by_id[a]
    suit_hash = suit_hash + suitbit_by_id[b]
    suit_hash = suit_hash + suitbit_by_id[c]
    suit_hash = suit_hash + suitbit_by_id[d]
    suit_hash = suit_hash + suitbit_by_id[e]
    suit_hash = suit_hash + suitbit_by_id[f]
    suit_hash = suit_hash + suitbit_by_id[g]

    if (suits[suit_hash]) {
      val suit_binary = Array(0) * 4

      suit_binary[a & 0x3] = suit_binary[a & 0x3] | binaries_by_id[a]
      suit_binary[b & 0x3] = suit_binary[b & 0x3] | binaries_by_id[b]
      suit_binary[c & 0x3] = suit_binary[c & 0x3] | binaries_by_id[c]
      suit_binary[d & 0x3] = suit_binary[d & 0x3] | binaries_by_id[d]
      suit_binary[e & 0x3] = suit_binary[e & 0x3] | binaries_by_id[e]
      suit_binary[f & 0x3] = suit_binary[f & 0x3] | binaries_by_id[f]
      suit_binary[g & 0x3] = suit_binary[g & 0x3] | binaries_by_id[g]

      return flush[suit_binary[suits[suit_hash] - 1]]
    }


    val quinary = Array.fill[Int](13)(0)

    quinary[a >> 2] = quinary[a >> 2] + 1
    quinary[b >> 2] = quinary[b >> 2] + 1
    quinary[c >> 2] = quinary[c >> 2] + 1
    quinary[d >> 2] = quinary[d >> 2] + 1
    quinary[e >> 2] = quinary[e >> 2] + 1
    quinary[f >> 2] = quinary[f >> 2] + 1
    quinary[g >> 2] = quinary[g >> 2] + 1

    return noflush7[hash_quinary(quinary, 13, 7)]
    ]
  }

  def evaluate_omaha_cards(c1: Int, c2: Int, c3: Int, c4: Int, c5: Int, h1: Int, h2: Int, h3: Int, h4: Int) {
    value_flush = 10000
    val suit_count_board = Array(0, 0, 0, 0)
    val suit_count_hole = Array(0, 0, 0, 0)

    suit_count_board[c1 & 0x3] = suit_count_board[c1 & 0x3] + 1
    suit_count_board[c2 & 0x3] = suit_count_board[c2 & 0x3] + 1
    suit_count_board[c3 & 0x3] = suit_count_board[c3 & 0x3] + 1
    suit_count_board[c4 & 0x3] = suit_count_board[c4 & 0x3] + 1
    suit_count_board[c5 & 0x3] = suit_count_board[c5 & 0x3] + 1

    suit_count_hole[h1 & 0x3] = suit_count_hole[h1 & 0x3] + 1
    suit_count_hole[h2 & 0x3] = suit_count_hole[h2 & 0x3] + 1
    suit_count_hole[h3 & 0x3] = suit_count_hole[h3 & 0x3] + 1
    suit_count_hole[h4 & 0x3] = suit_count_hole[h4 & 0x3] + 1

    for (i <- 0 to 4) {
      if (suit_count_board[i] >= 3 && suit_count_hole[i] >= 2) {
        val suit_binary_board = Array(0, 0, 0, 0)


        suit_binary_board[c1 & 0x3] = suit_binary_board[c1 & 0x3] | binaries_by_id[c1]
        suit_binary_board[c2 & 0x3] = suit_binary_board[c2 & 0x3] | binaries_by_id[c2]
        suit_binary_board[c3 & 0x3] = suit_binary_board[c3 & 0x3] | binaries_by_id[c3]
        suit_binary_board[c4 & 0x3] = suit_binary_board[c4 & 0x3] | binaries_by_id[c4]
        suit_binary_board[c5 & 0x3] = suit_binary_board[c5 & 0x3] | binaries_by_id[c5]

        val suit_binary_hole = Array(0, 0, 0, 0)
        suit_binary_hole[h1 & 0x3] = suit_binary_hole[h1 & 0x3] | binaries_by_id[h1]
        suit_binary_hole[h2 & 0x3] = suit_binary_hole[h2 & 0x3] | binaries_by_id[h2]
        suit_binary_hole[h3 & 0x3] = suit_binary_hole[h3 & 0x3] | binaries_by_id[h3]
        suit_binary_hole[h4 & 0x3] = suit_binary_hole[h4 & 0x3] | binaries_by_id[h4]
      }

      if (suit_count_board[i] == 3 and suit_count_hole[i] == 2) {
        val value_flush = flush[suit_binary_board[i] | suit_binary_hole[i]]
      } else {
        val padding = Array(0x0000, 0x2000, 0x6000)

        suit_binary_board[i] = suit_binary_board[i] | padding[5 - suit_count_board[i]]
        suit_binary_hole[i] = suit_binary_hole[i] | padding[4 - suit_count_hole[i]]

        val board_hash = hash_binary(suit_binary_board[i], 5)
        val hole_hash = hash_binary(suit_binary_hole[i], 4)

        value_flush = flushomaha[board_hash * 1365 + hole_hash]
      }


      val quinary_board = Array(0) * 13
      val quinary_hole = Array(0) * 13

      quinary_board[c1 >> 2] += 1
      quinary_board[c2 >> 2] += 1
      quinary_board[c3 >> 2] += 1
      quinary_board[c4 >> 2] += 1
      quinary_board[c5 >> 2] += 1

      quinary_hole[h1 >> 2] += 1
      quinary_hole[h2 >> 2] += 1
      quinary_hole[h3 >> 2] += 1
      quinary_hole[h4 >> 2] += 1

      val board_hash = hash_quinary(quinary_board, 13, 5)
      val hole_hash = hash_quinary(quinary_hole, 13, 4)

      val value_noflush = noflushomaha[board_hash * 1820 + hole_hash]

      if (value_flush < value_noflush) {
        return value_flush
      }
      else {
        return value_noflush
      }
    }
  }
}
