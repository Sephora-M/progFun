object excercie3 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  println("Welcome to the Scala worksheet");$skip(213); 
  
  def countChange(money: Int, coins: List[Int]): Int = {
  if (coins.isEmpty) 0
  else if (money ==0) 1
  else if (money <0) 0
  else (countChange(money, coins.tail) + countChange(money-coins.head, coins))
  };System.out.println("""countChange: (money: Int, coins: List[Int])Int""");$skip(33); val res$0 = 
  
 countChange(10, List(5,2,1));System.out.println("""res0: Int = """ + $show(res$0))}
}