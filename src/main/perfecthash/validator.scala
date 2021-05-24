package main

import scala.util.matching.Regex

class validator {
    val GAMES: Array[String] = Array("texas-holdem", "omaha-holdem", "five-card-draw")
    val LENGTH_MAP = Map(
      "texas-holdem" -> Array(5, 2, 4),
      "omaha-holdem" -> Array(5, 4, 8),
      "five-card-draw" -> Array(5, 5, 10)
    )

  def validateHands(gameType: String, hands: String): Boolean = {
    var ret:Boolean = True
    for (i <- 0 to hands.length) {
      if (hands[i].length != LENGTH_MAP[gameType][2]) {
        ret = False
      }
    }
    ret
  }

  def validateInput(cards: String): Boolean = {
    var ret:Boolean = True
    val cardsPattern: Regex = "[AKQJT2-9][hdcs]".r
    if (cardsPattern.findAllIn(cards).length < cards.length / 2) {
      ret = False
    }
    ret
  }

  def validateUnique(cardsList: Array[String]): Boolean = {
    var ret:Boolean = True
    if (cardsList.toSet.size < cardsList.length) {
      ret = False
    }
    ret
  }

  def validateLength(gameType: String, cardsList: Array[String]): Boolean = {
    var ret:Boolean = True
    if (cardsList.slice(0, 5).length % LENGTH_MAP[gameType][0]
      && cardsList.slice(5, cardsList.length) % LENGTH_MAP[gameType][1]) {
      ret = False
    }
    ret
  }
}
