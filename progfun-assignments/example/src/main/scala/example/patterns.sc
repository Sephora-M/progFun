package example

object patterns {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def f(list: Any) = list match {
    case a :: b :: Nil => true
    case _             => false
  }                                               //> f: (list: Any)Boolean

  def g(list: Any) = list match {
    case (a :: b) :: c => true
    case _             => false
  }                                               //> g: (list: Any)Boolean

  val cases = List(1 :: 2 :: 3 :: Nil,
    List(1 :: 2 :: 3 :: Nil),
    (1 :: Nil) :: (2 :: Nil) :: Nil,
    List(1, 2, 3) :: List(3, 4, 5),
    Nil :: Nil,
    (1 :: Nil) :: Nil,
    List(1, 2) :: List(3),
    List(1, 2) :: List(3, 4) :: Nil)              //> cases  : List[List[Any]] = List(List(1, 2, 3), List(List(1, 2, 3)), List(Lis
                                                  //| t(1), List(2)), List(List(1, 2, 3), List(3, 4, 5)), List(List()), List(List(
                                                  //| 1)), List(List(1, 2), 3), List(List(1, 2), List(3, 4)))

  def printSolution() {
    for (c <- cases) {
      println("f("+ c +") = "+ f(c))
      println("g("+ c +") = "+ g(c))
    }
  }                                               //> printSolution: ()Unit
  
  printSolution()                                 //> f(List(1, 2, 3)) = false
                                                  //| g(List(1, 2, 3)) = false
                                                  //| f(List(List(1, 2, 3))) = false
                                                  //| g(List(List(1, 2, 3))) = true
                                                  //| f(List(List(1), List(2))) = true
                                                  //| g(List(List(1), List(2))) = true
                                                  //| f(List(List(1, 2, 3), List(3, 4, 5))) = true
                                                  //| g(List(List(1, 2, 3), List(3, 4, 5))) = true
                                                  //| f(List(List())) = false
                                                  //| g(List(List())) = false
                                                  //| f(List(List(1))) = false
                                                  //| g(List(List(1))) = true
                                                  //| f(List(List(1, 2), 3)) = true
                                                  //| g(List(List(1, 2), 3)) = true
                                                  //| f(List(List(1, 2), List(3, 4))) = true
                                                  //| g(List(List(1, 2), List(3, 4))) = true
}