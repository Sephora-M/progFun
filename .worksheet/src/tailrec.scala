object tailrec {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(123); 
  def product(f: Int => Int)( a: Int, b: Int) : Int =
      if (a > b) 1
      else f(a)*product(f)(a+1,b);System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(26); val res$0 = 
    product(x=> x*x)(3,4);System.out.println("""res0: Int = """ + $show(res$0));$skip(53); 
	def factorial(n: Int) : Int =
	product(x => x)(1,n);System.out.println("""factorial: (n: Int)Int""");$skip(16); val res$1 = 
	
	factorial(5);System.out.println("""res1: Int = """ + $show(res$1));$skip(166); 
	
	def mapReduce(f: Int => Int, combine :(Int, Int) => Int, zero: Int)(a: Int, b:Int) : Int =
		if (a>b) zero
		else combine(f(a),mapReduce(f, combine, zero)(a+1,b));System.out.println("""mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int)Int""");$skip(45); val res$2 = 
		
		mapReduce(x=> x*x, (u,v)=> u*v, 1)(3,4);System.out.println("""res2: Int = """ + $show(res$2))}
}