

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case y :: ys =>
    if (n == 0) ys
    else y :: removeAt(n - 1, ys)
}

val xs = List('a', 'b', 'c', 'd')
removeAt(2, xs)