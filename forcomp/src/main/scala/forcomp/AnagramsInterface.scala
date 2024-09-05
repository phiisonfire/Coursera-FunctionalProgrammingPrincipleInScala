package forcomp

/**
 * The interface used by the grading infrastructure. Do not change signatures
 * or your submission will fail with a NoSuchMethodError.
 */
trait AnagramsInterface:
  def wordOccurrences(w: String): List[(Char, Int)] // get the Occurrences of a Word
  def sentenceOccurrences(s: List[String]): List[(Char, Int)] // get the Occurrences of a Sentence
  def dictionaryByOccurrences: Map[List[(Char, Int)], List[String]] // Map(occurrence1 -> List(String1, String2), occurrence2 -> ...)
  def wordAnagrams(word: String): List[String] // get all Anagrams (Words) of a Word
  def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] // get all possible subsets of a Occurrences
  def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] // subtract two Occurrences
  def sentenceAnagrams(sentence: List[String]): List[List[String]]
  /*
  * sen = List("I", "love", "you")
  * senOccurrences = List(i -> 1, l -> 1, o -> 2, v -> 1, e -> 1, u -> 1) -> n combs
  * smallerSenOccurrences_1 = senOccurrences - comb_1 (in combs) -> x combs
  *   smallerSenOccurrences_1_1 = smallerSenOccurrences_1 - comb_1_1 (in x_combs)
  *   smallerSenOccurrences_1_2 = smallerSenOccurrences_1 - comb_1_2 (in x_combs)
  *   ...
  *   smallerSenOccurrences_1_x = smallerSenOccurrences_1 - comb_1_x (in x_combs)
  *   ...
  *   emptySentence -> anagram = List(Nil)
  * smallerSenOccurrences_2 = senOccurrences - comb_2 (in combs) -> y combs
  * ...
  * smallerSenOccurrences_n = senOccurrences - comb_n (in combs) -> z combs
  * ...
  
  
  
  * emptySentence -> anagram = List(Nil)
  * 
  * combs = [occ1, occ2, occ3, occ4, occ5, ...]
  * 
  *
  * */
