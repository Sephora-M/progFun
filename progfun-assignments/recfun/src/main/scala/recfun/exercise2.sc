object exercise2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def balance(chars: List[Char]): Boolean = {
    def matching(open: Int, chars: List[Char]): Boolean = {
    
    if(open<0) false
    
    else if (chars.isEmpty) (open==0)
   
    else if (chars.head == '(') matching(open+1, chars.tail)
    else if (chars.head == ')') matching(open-1, chars.tail)
    
    else matching(open, chars.tail)
    
    }
    matching(0, chars)
	}                                         //> balance: (chars: List[Char])Boolean


println(balance("Voila ca ((((marc)h)e) pas)".toList))
                                                  //> true

}