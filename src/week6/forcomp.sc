package week6

import java.io.File

object forcomp {
	type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy(c => c).toList.unzip._2.sortWith(_.head < _.head) map (e => (e.head, e.length))
  }
	def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences((s flatMap (x=>x)).mkString)
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))
   val dictionary: List[Word] = loadDictionary
	
	val b: Word = "Robert"
	val a: Word = "abracadabra"
	val c: Word = "coquelico"
	wordOccurrences(b)
 	val s: Sentence = List(a,b,c)
 
 val sw = (s flatMap (x=>x)).mkString
 wordOccurrences(sw)
                      
                      
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
			
	}
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
	}
	val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
	val y = List(('r', 1))
	
	val sub = subtract(x,y)
  val nimportquoi = combinations( List(('a', 2), ('b', 2), ('t',2)))
  val rien = combinations(Nil)
  
   def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occurrences = sentenceOccurrences(sentence)
    if (sentence.isEmpty) List()
    else
      for {
        x <- combinations(occurrences)
        w <- dictionaryByOccurrences(x)
        y = subtract(occurrences, x)
      } yield List(w):::sentenceAnagrams(sentence - (w))
  }
}