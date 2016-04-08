object session {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 

  def abs(x: Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(308); 
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(10); val res$0 = 
  sqrt(4);System.out.println("""res0: Double = """ + $show(res$0))}

}