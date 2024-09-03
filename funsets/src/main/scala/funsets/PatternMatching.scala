package funsets

object PatternMatching {
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr
  case class Product(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)

  def show(e: Expr): String = e match
    case Number(n) => n.toString
    case Var(x) => x
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Product(e1, e2) => s"${showP(e1)} * ${showP(e2)}"

  def showP(e: Expr): String = e match
    case Sum(e1, e2) => s"(${show(e)})"
    case _ => show(e)

  @main def run(): Unit = {
    val expr = Product(Sum(Number(2), Number(3)), Number(4))
    println(show(expr))
  }
}
