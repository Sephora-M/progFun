package interpreter

object Main extends App {
  // TODO: Insert code for the REPL
  import java.util._
  import Lisp._

  val sc: Scanner = new Scanner(System.in)
  var str: String = ""
  do {
    print("lisp>");
    str = sc.nextLine()
    if (!str.equals("exit")) {
      println(evaluate(str))
    }
  } while (!str.equals("exit"))
   
}

object LispCode {
  // TODO: implement the function `concat` in Lisp. Write it as a String, and test it in your REPL
  val concat = "def (concat L1 L2) (if (null? L1) L2 (cons (car L1) (concat (cdr L1) L2)))"
  // TODO: implement the function `reverse` in Lisp. Write it as a String, and test it in your REPL
  val reverse = "def (reverse L) (if (null? L) L (concat (reverse (cdr L)) (cons (car L) (quote()))))"

  // TODO: 
	  /*
	   * Exercises that we did in class today which you should fully understand and try at home:
	● size function, which traverses a list and computes its length
	● tf function which takes a list, representing lisp code, and replaces all occurrences
	of "true" with `1` in lisp, in case we were interested in evaluating the translated expression. 
	For example, it would take as an argument (quote (if (= pred true) 20 0)) 
	and it would return: (if (= pred 1) 20 0).
	Make sure that your function can handle `true` symbols in subexpressions!
	Notes
	● make sure you know how to construct, traverse, and call functions on lists
	● make sure you understand quoting
	○ quoting allows you to bypass evaluation
	○ it allows you to turn rather arbitrary expressions into lists, for example:
	             lisp> (quote (blah blah ajs 9 yesyes yeehaw))
	             (blah blah ajs 9 yesyes yeehaw)
	○ this is also true for lisp code-- we can turn code into a list (i.e. data) 
	without evaluating it, and then we can traverse or translate the list 
	into a different list, which could also represent a program.
	● lisp and typing, and how if affects your programs
	○ types in lisp aren't checked statically, and only sometimes, if necessary,
	dynamically
	○ remember that lists can be heterogeneous in lisp, for example, this is perfectly
	valid....
	             lisp> (cons 1 (cons (quote apple) (cons (cons 2 (cons 3 nil)) nil )))
	             (1 apple (2 3))
	○ imagine if you were writing a function that would take a list and traverse it, 
	assuming its elements were numbers. your implementation could easily become incorrect 
	if you aren't careful to check that its argument is actually a list, and to handle all 
	other cases carefully. For example, it's conceivable that your function could be recursively
	 passed a single element or a list of lists, which is completely valid in Lisp (unlike Scala).
	   */
    
        //(def (tf L) (if (null? L) () (if (= (car L) 4) (cons 1 (tf (cdr L))) (cons ( (car L) (tf (cdr L)))) (tf quote(4 2 3) ) )))

    
    val truesize = "(def (size L x) (if (null? L) x (size (cdr L) (+ 1 x))) (size (quote (1 2)) 0))"
      val tf4 = "(def (tf L) (cond ((null? L) 0) ((= (quote (car L)) 'true) (cons 1 (tf (cdr L)))) (else (cons (quote (car L)) (tf (cdr L))))) (tf (quote (1 2 true))))"
   val tf3 = "   	(def (tf L) (cond ((null? L) 0) ((= 1 (size (quote (car L)) 0)) ((if (= (quote (car L)) 'true))  (cons 1 (tf (cdr L))) (cons (quote (car L)) (tf (cdr L)))))) (tf (quote (1 2 true))))"
     val tf2 = "(def (tf L) (if (null? L) () (val t (car L) if (= t 'true) (cons(1 (tf (cdr L)))) (concat (tf t) (tf cdr L)))))"
    val tf = "(def (tf L) (if (null? L) () (val noteval (quote (car L)) (if (= noteval 'true) (cons(1 (tf (cdr L)))) (cons(noteval (tf (cdr L)))))) (tf (quote (true 1 2))))"
}