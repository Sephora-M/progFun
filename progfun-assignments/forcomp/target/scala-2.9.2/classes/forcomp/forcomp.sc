package forcomp
import common._

import java.io.File
import scala.collection.immutable._

object forcomp {
	type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy(c => c).toList.unzip._2.sortWith(_.head < _.head) map (e => (e.head, e.length))
  }                                               //> wordOccurrences: (w: forcomp.forcomp.Word)forcomp.forcomp.Occurrences
	def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences((s flatMap (x=>x)).mkString)    //> sentenceOccurrences: (s: forcomp.forcomp.Sentence)forcomp.forcomp.Occurrence
                                                  //| s
 // lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))
   val dictionary: List[Word] = loadDictionary    //> dictionary  : List[forcomp.forcomp.Word] = List(Aarhus, Aaron, Ababa, aback,
                                                  //|  abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased
                                                  //| , abasement, abasements, abases, abash, abashed, abashes, abashing, abasing,
                                                  //|  abate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, 
                                                  //| abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, 
                                                  //| abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdomina
                                                  //| l, abduct, abducted, abduction, abductions, abductor, abductors, abducts, Ab
                                                  //| e, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, 
                                                  //| aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhor
                                                  //| red, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding,
                                                  //|  Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abjection
                                                  //| s, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated
                                                  //| , ablates, ablating, abl
                                                  //| Output exceeds cutoff limit.
	
	val b: Word = "Robert"                    //> b  : forcomp.forcomp.Word = Robert
	val a: Word = "abracadabra"               //> a  : forcomp.forcomp.Word = abracadabra
	val c: Word = "coquelico"                 //> c  : forcomp.forcomp.Word = coquelico
	wordOccurrences(b)                        //> res0: forcomp.forcomp.Occurrences = List((b,1), (e,1), (o,1), (r,2), (t,1))
 	val s: Sentence = List(a,b,c)             //> s  : forcomp.forcomp.Sentence = List(abracadabra, Robert, coquelico)
 
 val sw = (s flatMap (x=>x)).mkString             //> sw  : String = abracadabraRobertcoquelico
 wordOccurrences(sw)                              //> res1: forcomp.forcomp.Occurrences = List((a,5), (b,3), (c,3), (d,1), (e,2), 
                                                  //| (i,1), (l,1), (o,3), (q,1), (r,4), (t,1), (u,1))
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))
                                                  //> dictionaryByOccurrences  : scala.collection.immutable.Map[forcomp.forcomp.Oc
                                                  //| currences,List[forcomp.forcomp.Word]] = <lazy>
                      
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
	}                                         //> combinations: (occurrences: forcomp.forcomp.Occurrences)List[forcomp.forcom
                                                  //| p.Occurrences]
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
	}                                         //> subtract: (x: forcomp.forcomp.Occurrences, y: forcomp.forcomp.Occurrences)f
                                                  //| orcomp.forcomp.Occurrences
	val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
	val y = List(('r', 1))                    //> y  : List[(Char, Int)] = List((r,1))
	
	val sub = subtract(x,y)                   //> sub  : forcomp.forcomp.Occurrences = List((a,1), (d,1), (l,1))
  val nimportquoi = combinations( List(('a', 2), ('b', 2), ('t',2)))
                                                  //> nimportquoi  : List[forcomp.forcomp.Occurrences] = List(List(), List((t,1))
                                                  //| , List((t,2)), List((b,1)), List((b,1), (t,1)), List((b,1), (t,2)), List((b
                                                  //| ,2)), List((b,2), (t,1)), List((b,2), (t,2)), List((a,1)), List((a,1), (t,1
                                                  //| )), List((a,1), (t,2)), List((a,1), (b,1)), List((a,1), (b,1), (t,1)), List
                                                  //| ((a,1), (b,1), (t,2)), List((a,1), (b,2)), List((a,1), (b,2), (t,1)), List(
                                                  //| (a,1), (b,2), (t,2)), List((a,2)), List((a,2), (t,1)), List((a,2), (t,2)), 
                                                  //| List((a,2), (b,1)), List((a,2), (b,1), (t,1)), List((a,2), (b,1), (t,2)), L
                                                  //| ist((a,2), (b,2)), List((a,2), (b,2), (t,1)), List((a,2), (b,2), (t,2)))
  val rien = combinations(Nil)                    //> rien  : List[forcomp.forcomp.Occurrences] = List()
  
  
  
  
  
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

  }                                               //> sentenceAnagrams: (sentence: forcomp.forcomp.Sentence)List[forcomp.forcomp.
                                                  //| Sentence]
  
  val sentence = List("Linux", "rulez")           //> sentence  : List[java.lang.String] = List(Linux, rulez)
  
  val pleaseberight = sentenceAnagrams(sentence)  //> pleaseberight  : List[forcomp.forcomp.Sentence] = List(List(Zulu, Lin, Rex)
                                                  //| , List(Zulu, nil, Rex), List(Zulu, Rex, Lin), List(Zulu, Rex, nil), List(nu
                                                  //| ll, Uzi, Rex), List(null, Rex, Uzi), List(Uzi, null, Rex), List(Uzi, Rex, n
                                                  //| ull), List(Lin, Zulu, Rex), List(Lin, Rex, Zulu), List(nil, Zulu, Rex), Lis
                                                  //| t(nil, Rex, Zulu), List(Linux, rulez), List(Rex, Zulu, Lin), List(Rex, Zulu
                                                  //| , nil), List(Rex, null, Uzi), List(Rex, Uzi, null), List(Rex, Lin, Zulu), L
                                                  //| ist(Rex, nil, Zulu), List(rulez, Linux))
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