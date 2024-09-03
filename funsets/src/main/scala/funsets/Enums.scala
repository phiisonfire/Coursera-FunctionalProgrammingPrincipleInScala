package funsets

enum Expr: // pure data definition ~ algebraic data types (ADTs)
  case Var(s: String)
  case Number(n: Int)
  case Sum(e1: Expr, e2: Expr)
  case Prod(e1: Expr, e2: Expr)

enum DayOfWeek:
  case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

def isWeekend(day: DayOfWeek): Boolean = day match
  case DayOfWeek.Saturday | DayOfWeek.Sunday => true
  case _ => false

enum Direction(val dx: Int, val dy: Int):
  case Right extends Direction(1, 0) // ordinal = 0
  case Up extends Direction(0, 1) // ordinal = 1
  case Left extends Direction(-1, 0) // ordinal = 2
  case Down extends Direction(0, -1) // ordinal = 3

  // return an array containing all the enum simple cases (parameterized cases do not included)
  val directions: Array[Direction] = Direction.values

  def leftTurn = Direction.values((ordinal + 1) % 4)

end Direction

val r = Direction.Right
val u = r.leftTurn
val v = (u.dx, u.dy)




