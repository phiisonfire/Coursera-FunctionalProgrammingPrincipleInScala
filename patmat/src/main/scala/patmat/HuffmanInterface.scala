package patmat

/**
 * The interface used by the grading infrastructure. Do not change signatures
 * or your submission will fail with a NoSuchMethodError.
 */
trait HuffmanInterface:
  def weight(tree: CodeTree): Int // get the weight of a CodeTree
  def chars(tree: CodeTree): List[Char] // get chars contained in a CodeTree
  def times(chars: List[Char]): List[(Char, Int)] // get frequency of each char in list chars
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] //
  def singleton(trees: List[CodeTree]): Boolean // check if a list of CodeTree contains only 1 element
  def combine(trees: List[CodeTree]): List[CodeTree] // combine 2 first element and add back to the list, reserve order
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree]
  def createCodeTree(chars: List[Char]): CodeTree // create codetree
  def decode(tree: CodeTree, bits: List[Int]): List[Char]
  def decodedSecret: List[Char]
  def encode(tree: CodeTree)(text: List[Char]): List[Int]
  def convert(tree: CodeTree): List[(Char, List[Int])]
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Int]
  def frenchCode: CodeTree
  def secret: List[Int]
