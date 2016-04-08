package forcomp
import common._

import java.io.File
import scala.collection.immutable._

object forcomp {
	type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(357); 
  def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy(c => c).toList.unzip._2.sortWith(_.head < _.head) map (e => (e.head, e.length))
  };System.out.println("""wordOccurrences: (w: forcomp.forcomp.Word)forcomp.forcomp.Occurrences""");$skip(100); 
	def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences((s flatMap (x=>x)).mkString);System.out.println("""sentenceOccurrences: (s: forcomp.forcomp.Sentence)forcomp.forcomp.Occurrences""");$skip(160); 
 // lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))
   val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[forcomp.forcomp.Word] = """ + $show(dictionary ));$skip(26); 
	
	val b: Word = "Robert";System.out.println("""b  : forcomp.forcomp.Word = """ + $show(b ));$skip(29); 
	val a: Word = "abracadabra";System.out.println("""a  : forcomp.forcomp.Word = """ + $show(a ));$skip(27); 
	val c: Word = "coquelico";System.out.println("""c  : forcomp.forcomp.Word = """ + $show(c ));$skip(20); val res$0 = 
	wordOccurrences(b);System.out.println("""res0: forcomp.forcomp.Occurrences = """ + $show(res$0));$skip(32); 
 	val s: Sentence = List(a,b,c);System.out.println("""s  : forcomp.forcomp.Sentence = """ + $show(s ));$skip(40); 
 
 val sw = (s flatMap (x=>x)).mkString;System.out.println("""sw  : String = """ + $show(sw ));$skip(21); val res$1 = 
 wordOccurrences(sw);System.out.println("""res1: forcomp.forcomp.Occurrences = """ + $show(res$1));$skip(111); 
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w));System.out.println("""dictionaryByOccurrences  : scala.collection.immutable.Map[forcomp.forcomp.Occurrences,List[forcomp.forcomp.Word]] = <lazy>""");$skip(1593); 
                      
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
    	def combinations0(occ0: Occurrences): List[Occurrences] = {(
    	for {
      occ <- occurrences
      n <- 0 to occ._2
      rest = occurrences.filter(y => y._1 > occ._1)
      combi <- if (rest.isEmpty) List(List()) else combinations(rest)
    } yield {
      if (n == 0) combi else (occ._1, n) :: combi
    }).distinct
    }
			combinations0(occurrences)
	};System.out.println("""combinations: (occurrences: forcomp.forcomp.Occurrences)List[forcomp.forcomp.Occurrences]""");$skip(445); 
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
  	(subset(x,y) ::: excl(x,y)).sortWith(_._1 < _._1)
	};System.out.println("""subtract: (x: forcomp.forcomp.Occurrences, y: forcomp.forcomp.Occurrences)forcomp.forcomp.Occurrences""");$skip(54); 
	val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1));System.out.println("""x  : List[(Char, Int)] = """ + $show(x ));$skip(24); 
	val y = List(('r', 1));System.out.println("""y  : List[(Char, Int)] = """ + $show(y ));$skip(27); 
	
	val sub = subtract(x,y);System.out.println("""sub  : forcomp.forcomp.Occurrences = """ + $show(sub ));$skip(69); 
  val nimportquoi = combinations( List(('a', 2), ('b', 2), ('t',2)));System.out.println("""nimportquoi  : List[forcomp.forcomp.Occurrences] = """ + $show(nimportquoi ));$skip(31); 
  val rien = combinations(Nil);System.out.println("""rien  : List[forcomp.forcomp.Occurrences] = """ + $show(rien ));$skip(510); 
  
  
  
  
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def occurrencesAnagrams(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else for {
        combi <- combinations( occurrences )
        w <- dictionaryByOccurrences getOrElse (combi, Nil)
        s <- occurrencesAnagrams( subtract(occurrences,wordOccurrences(w)) )
        //if !combi.isEmpty
      } yield w :: s
    }

    occurrencesAnagrams( sentenceOccurrences(sentence) )

  };System.out.println("""sentenceAnagrams: (sentence: forcomp.forcomp.Sentence)List[forcomp.forcomp.Sentence]""");$skip(43); 
  
  val sentence = List("Linux", "rulez");System.out.println("""sentence  : List[java.lang.String] = """ + $show(sentence ));$skip(52); 
  
  val pleaseberight = sentenceAnagrams(sentence);System.out.println("""pleaseberight  : List[forcomp.forcomp.Sentence] = """ + $show(pleaseberight ))}
  /*-List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      -List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      -List("Lin", "Rex", "Zulu"),
      -List("nil", "Rex", "Zulu"),
      -List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
     - List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      -List("rulez", "Linux"),
      -List("Linux", "rulez")
  */
                                                  
}