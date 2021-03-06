package common
object forcomp {
	type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(282); 
  def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase.toList.groupBy(c => c).toList.unzip._2.sortWith(_.head < _.head) map (e => (e.head, e.length))
  };System.out.println("""wordOccurrences: (w: common.forcomp.Word)common.forcomp.Occurrences""");$skip(26); 
	
	val b: Word = "Robert";System.out.println("""b  : common.forcomp.Word = """ + $show(b ));$skip(29); 
	val a: Word = "abracadabra";System.out.println("""a  : common.forcomp.Word = """ + $show(a ));$skip(27); 
	val c: Word = "coquelico";System.out.println("""c  : common.forcomp.Word = """ + $show(c ));$skip(20); val res$0 = 
	wordOccurrences(b);System.out.println("""res0: common.forcomp.Occurrences = """ + $show(res$0));$skip(31); 
 val s: Sentence = List(a,b,c);System.out.println("""s  : common.forcomp.Sentence = """ + $show(s ));$skip(37); 
 
 
 val p = "salut".toList.mkString;System.out.println("""p  : String = """ + $show(p ));$skip(40); 
 
 val sw = (s flatMap (x=>x)).mkString;System.out.println("""sw  : String = """ + $show(sw ));$skip(21); val res$1 = 
 wordOccurrences(sw);System.out.println("""res1: common.forcomp.Occurrences = """ + $show(res$1));$skip(48); 
 	  val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[common.forcomp.Word] = """ + $show(dictionary ));$skip(129); 
 	                
	lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w));System.out.println("""dictionaryByOccurrences  : Map[common.forcomp.Occurrences,List[common.forcomp.Word]] = <lazy>""")}
  
}