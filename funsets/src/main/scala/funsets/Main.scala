package funsets

object Main extends App:
  import FunSets.*
  println(contains(singletonSet(1), 1))
  def testSet(x: Int): Boolean = x > 0 && x < 10
  def f(x: Int): Int = x * 2
  printSet(testSet)
  printSet(map(testSet, f))

