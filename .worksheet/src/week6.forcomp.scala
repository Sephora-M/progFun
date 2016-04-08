package week6

import java.io.File

object forcomp {
	type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(303); 
  def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy(c => c).toList.unzip._2.sortWith(_.head < _.head) map (e => (e.head, e.length))
  };System.out.println("""wordOccurrences: (w: week6.forcomp.Word)week6.forcomp.Occurrences""");$skip(100); 
	def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences((s flatMap (x=>x)).mkString);System.out.println("""sentenceOccurrences: (s: week6.forcomp.Sentence)week6.forcomp.Occurrences""");$skip(111); 
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w));System.out.println("""dictionaryByOccurrences  : Map[week6.forcomp.Occurrences,List[week6.forcomp.Word]] = <lazy>""");$skip(47); 
   val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[week6.forcomp.Word] = """ + $show(dictionary ));$skip(26); 
	
	val b: Word = "Robert";System.out.println("""b  : week6.forcomp.Word = """ + $show(b ));$skip(29); 
	val a: Word = "abracadabra";System.out.println("""a  : week6.forcomp.Word = """ + $show(a ));$skip(27); 
	val c: Word = "coquelico";System.out.println("""c  : week6.forcomp.Word = """ + $show(c ));$skip(20); val res$0 = 
	wordOccurrences(b);System.out.println("""res0: week6.forcomp.Occurrences = """ + $show(res$0));$skip(32); 
 	val s: Sentence = List(a,b,c);System.out.println("""s  : week6.forcomp.Sentence = """ + $show(s ));$skip(40); 
 
 val sw = (s flatMap (x=>x)).mkString;System.out.println("""sw  : String = """ + $show(sw ));$skip(21); val res$1 = 
 wordOccurrences(sw);System.out.println("""res1: week6.forcomp.Occurrences = """ + $show(res$1));$skip(1899); 
                      
                      
 /**  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   List(
   List((a,1)),
   List((a,2), (b,2)),
   List((a,1), (b,2)),
   List((a,2)),
   List((a,2), (b,1)),
   List((b,2)),
   List((b,1)),
   List((a,1), (b,1)),
   List())
   
  
   List(
   List((a,1)),
   List((a,2)),
   
   List((b,2)),
   List((b,1)),
   
   List((t,2)),
   List((t,1)),
    
   List((a,1), (t,1)),
   List((a,2), (b,1)),
   List((a,2), (b,2)),
   List((a,1), (b,2)),
   List((a,1), (t,2)),
   List((a,2), (t,1)),
   List((a,1), (b,1)),
   List((a,2), (t,2)),
    
   List((a,1), (b,1), (t,1)),
   List((a,1), (b,2), (t,2)),
   List((a,2), (b,1), (t,1)),
   List((a,2), (b,1), (t,2)),
   List((a,2), (b,2), (t,2)),
   List((a,1), (b,2), (t,1)),
   List((a,1), (b,1), (t,2)),
   List((a,2), (b,2), (t,1)),
   
   List()
   )
   OK
   */
 def combinations(occurrences: Occurrences): List[Occurrences] = {
    	def combinations0(occ0: Occurrences): Set[Occurrences] = {
    	if (occ0.isEmpty) Set(List())
  			else{
  	  		for {
  	    		combi <- combinations(occ0.tail)
  	    		n <- 1 to occurrences.head._2
  	    	
  	  		} yield (combi:::List((occ0.head._1,n))).sortWith(_._1 < _._1)}.toSet
			}
			
			def combinations1(occ1: Occurrences): Set[Occurrences] = {
			if (occ1.isEmpty) Set(List())
				else{
				 for{
				 	occ <- occ1
				 	n <- 1 to occ._2
				 } yield List((occ._1,n))}.toSet
	 		
			}
			
			
			if (occurrences.isEmpty) List(List()) else
			(combinations0(occurrences) ++ combinations1(occurrences)).toList ::: List(List())
			
	};System.out.println("""combinations: (occurrences: week6.forcomp.Occurrences)List[week6.forcomp.Occurrences]""");$skip(421); 
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
		def subset(xs: Occurrences, ys: Occurrences): Occurrences = {
		  for {
		    x0 <- xs
		    y0 <- ys
		    if ((x0._1.equals(y0._1) && (x0._2-y0._2 > 0)))
		  } yield (x0._1, x0._2 - y0._2)
 	 	}
  	def excl(xs: Occurrences, ys: Occurrences): Occurrences = {
  	  xs.filter(x => !ys.exists(y => (y._1.equals(x._1))))
  	}
  	subset(x,y) ::: excl(x,y)
	};System.out.println("""subtract: (x: week6.forcomp.Occurrences, y: week6.forcomp.Occurrences)week6.forcomp.Occurrences""");$skip(54); 
	val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1));System.out.println("""x  : List[(Char, Int)] = """ + $show(x ));$skip(24); 
	val y = List(('r', 1));System.out.println("""y  : List[(Char, Int)] = """ + $show(y ));$skip(27); 
	
	val sub = subtract(x,y);System.out.println("""sub  : week6.forcomp.Occurrences = """ + $show(sub ));$skip(69); 
  val nimportquoi = combinations( List(('a', 2), ('b', 2), ('t',2)));System.out.println("""nimportquoi  : List[week6.forcomp.Occurrences] = """ + $show(nimportquoi ));$skip(31); 
  val rien = combinations(Nil);System.out.println("""rien  : List[week6.forcomp.Occurrences] = """ + $show(rien ));$skip(350); 
  
   def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occurrences = sentenceOccurrences(sentence)
    if (sentence.isEmpty) List()
    else
      for {
        x <- combinations(occurrences)
        w <- dictionaryByOccurrences(x)
        y = subtract(occurrences, x)
      } yield List(w):::sentenceAnagrams(sentence - (w))
  };System.out.println("""sentenceAnagrams: (sentence: week6.forcomp.Sentence)List[week6.forcomp.Sentence]""")}
}