object excercie3 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def countChange(money: Int, coins: List[Int]): Int = {
  if (coins.isEmpty) 0
  else if (money ==0) 1
  else if (money <0) 0
  else (countChange(money, coins.tail) + countChange(money-coins.head, coins))
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  
 countChange(10, List(5,2,1))                     //> res0: Int = 10
}