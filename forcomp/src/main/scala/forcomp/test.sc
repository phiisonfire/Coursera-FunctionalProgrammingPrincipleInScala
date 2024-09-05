val sentence: List[String] = List("I", "Love", "Scala")
sentence.reduceLeft((word1, word2) => word1 + word2)

def extractSubsetsOfAOccurrencePair(pair: (Char, Int)): List[(Char, Int)] = pair match
  case (c, 0) => Nil
  case (c, i) => (c, i) :: extractSubsetsOfAOccurrencePair((c, i - 1))

val occurrences = List(('a', 2), ('b', 2))
val expandedOccurences = occurrences.map { case (char, count) =>
  (0 to count).map(i => (char, i)).toList.filter(_._2 != 0) :+ (char, 0)
}
type Occurrences = List[(Char, Int)]
def combinations(occurrences: Occurrences): List[Occurrences] = {
  // For each character and its count, generate all possible combinations
  val combs = occurrences.map { case (char, count) =>
    (for (i <- 1 to count) yield (char, i)).toList
  }

  // Compute all subsets of combinations by folding over the characters' combinations
  combs.foldLeft(List(List.empty[(Char, Int)])) { (acc, charsComb) =>
    acc ++ (for {
      subset <- acc
      charComb <- charsComb
    } yield subset :+ charComb)
  }
}
combinations(occurrences)

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val yMap = y.toMap
  x.foldLeft(Map.empty[Char, Int]) { case (acc, (c, occ)) => yMap.get(c) match
    case None => acc.updated(c, occ)
    case Some(value) => {
      if (occ - value == 0) then acc - c
      else acc.updated(c, occ - value)
    }
  }.toList
}

val x: Occurrences = List('a' -> 1, 'b' -> 2, 'c' -> 3)
val y: Occurrences = List('b' -> 2, 'c' -> 2)
subtract(x, y)
