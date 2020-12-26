type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

val w = "DAbcBacD"

w.toLowerCase

def wordOccurrences(w: Word) : Occurrences = {
  val lcw = w.toLowerCase.filter(c => c.isLetter)
  lcw.map(c => (c, lcw.count(_ == c))).distinct.sorted.toList
  //w.toLowerCase.filter(c => c.isLetter)
}

wordOccurrences(w)
w.toLowerCase.filter(c => c.isLetter)
(w.toLowerCase.filter(c => c.isLetter)).groupBy(c => c).mapValues(x=>x.length).toList.sorted


val words: Sentence =  List("Able", "was", "I", "ere", "I", "saw", "Elba" )

words.mkString

// either concatenate all strings into one and call wordOccurrences
// or call wordOccurrences on all strings and then sum by char
//wordOccurrences(s.mkString)
def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences((words foldLeft "") (_ + _))

def concat[T](xs: List[T], ys: List[T]) : List[T] =
  (xs foldRight ys) (_ :: _)

words.head
words.last
concat(List(words.head), List(words.last))

def concatenate[T](xs: List[T], ys: List[T]) : List[T] =
  (xs, ys) match {
  case (xs, Nil) => xs
  case (Nil, ys) => ys
  case (x :: xs1, y :: ys1) => concat(List(x), List(y)) ::: concatenate(xs1, ys1)
}


val s = sentenceOccurrences(words)

val foo = List( ('a', 3), ('b', 2))
val bar = List(('a', 1))

val ocs : List[Occurrences] = foo.map( x => (for (i <- 1 to x._2) yield (x._1,i)).toList)

def combinations(occurrences: Occurrences): List[Occurrences] = {
  val ocs : List[Occurrences] = occurrences.map( x => (for (i <- 1 to x._2) yield (x._1,i)).toList)
  ocs.foldRight (List[Occurrences](Nil)) ((x, y) => y ++ (for (i <- x; j <- y) yield i :: j))
}

combinations(foo)

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val (m,nm) = x.partition(a => y.exists(b => a._1 == b._1))
  val sub = for( (a,b) <- m.zip(y) if a._2 != b._2) yield (a._1,a._2-b._2)
  (nm ++ sub).sorted
}

//var y = for ( (xc, xn) <- foo; (yc, yn) <- bar; ) yield (xc, yc)
//val quux = foo zip bar

var k = foo.toMap

subtract(foo, bar)
