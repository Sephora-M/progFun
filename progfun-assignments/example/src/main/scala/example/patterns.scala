package example

object patterns2 {
  def f(list: Any) = list match {
    case a :: b :: Nil => true
    case _             => false
  }

  def g(list: Any) = list match {
    case (a :: b) :: c => true
    case _             => false
  }

  val cases = List(1 :: 2 :: 3 :: Nil,
    List(1 :: 2 :: 3 :: Nil),
    (1 :: Nil) :: (2 :: Nil) :: Nil,
    List(1, 2, 3) :: List(3, 4, 5),
    Nil :: Nil,
    (1 :: Nil) :: Nil,
    List(1, 2) :: List(3),
    List(1, 2) :: List(3, 4) :: Nil)

  def printSolution() {
    for (c <- cases) {
      println("f("+ c +") = "+ f(c))
      println("g("+ c +") = "+ g(c))
    }
  }
object Main extends App {
 printSolution()
}
}

