object exercise2 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  println("Welcome to the Scala worksheet");$skip(382); 
  
  def balance(chars: List[Char]): Boolean = {
    def matching(open: Int, chars: List[Char]): Boolean = {
    
    if(open<0) false
    
    else if (chars.isEmpty) (open==0)
   
    else if (chars.head == '(') matching(open+1, chars.tail)
    else if (chars.head == ')') matching(open-1, chars.tail)
    
    else matching(open, chars.tail)
    
    }
    matching(0, chars)
	};System.out.println("""balance: (chars: List[Char])Boolean""");$skip(57); 


println(balance("Voila ca ((((marc)h)e) pas)".toList))}

}