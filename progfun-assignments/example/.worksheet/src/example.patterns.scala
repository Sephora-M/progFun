package example

object patterns {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(78); 
  println("Welcome to the Scala worksheet");$skip(101); 
  def f(list: Any) = list match {
    case a :: b :: Nil => true
    case _             => false
  };System.out.println("""f: (list: Any)Boolean""");$skip(102); 

  def g(list: Any) = list match {
    case (a :: b) :: c => true
    case _             => false
  };System.out.println("""g: (list: Any)Boolean""");$skip(246); 

  val cases = List(1 :: 2 :: 3 :: Nil,
    List(1 :: 2 :: 3 :: Nil),
    (1 :: Nil) :: (2 :: Nil) :: Nil,
    List(1, 2, 3) :: List(3, 4, 5),
    Nil :: Nil,
    (1 :: Nil) :: Nil,
    List(1, 2) :: List(3),
    List(1, 2) :: List(3, 4) :: Nil);System.out.println("""cases  : List[List[Any]] = """ + $show(cases ));$skip(132); 

  def printSolution() {
    for (c <- cases) {
      println("f("+ c +") = "+ f(c))
      println("g("+ c +") = "+ g(c))
    }
  };System.out.println("""printSolution: ()Unit""");$skip(21); 
  
  printSolution()}
}