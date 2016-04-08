package week6

import java.io.File

object forcomp {
	type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy(c => c).toList.unzip._2.sortWith(_.head < _.head) map (e => (e.head, e.length))
  }                                               //> wordOccurrences: (w: week6.forcomp.Word)week6.forcomp.Occurrences
	
	val b: Word = "Robert"                    //> b  : week6.forcomp.Word = Robert
	val a: Word = "abracadabra"               //> a  : week6.forcomp.Word = abracadabra
	val c: Word = "coquelico"                 //> c  : week6.forcomp.Word = coquelico
	wordOccurrences(b)                        //> res0: week6.forcomp.Occurrences = List((b,1), (e,1), (o,1), (r,2), (t,1))
 	val s: Sentence = List(a,b,c)             //> s  : week6.forcomp.Sentence = List(abracadabra, Robert, coquelico)
 
 val sw = (s flatMap (x=>x)).mkString             //> sw  : String = abracadabraRobertcoquelico
 wordOccurrences(sw)                              //> res1: week6.forcomp.Occurrences = List((a,5), (b,3), (c,3), (d,1), (e,2), (i
                                                  //| ,1), (l,1), (o,3), (q,1), (r,4), (t,1), (u,1))
                      
                      
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
   */
 
 def combinations(occurrences: Occurrences): List[Occurrences] = List()
                                                  //> combinations: (occurrences: week6.forcomp.Occurrences)List[week6.forcomp.Oc
                                                  //| currences]

  
  val nimportquoi = combinations( List(('a', 2), ('b', 2), ('c',7) ))
                                                  //> nimportquoi  : List[week6.forcomp.Occurrences] = List()
}