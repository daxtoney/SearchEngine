class Query (strs: Seq[String]) extends Weighted[String] {
	var wordAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()

	def getItems(): Seq[String] = {
		strs
	}

	def getWeights(): Seq[Double] = {
		val ret = for(weight <- wordAndWeight) yield weight._2
		ret
	}

	def assignWeights(): Unit = {
		wordAndWeight = strs.map((_, 1.0))
	}
}

class DictionaryQuery(strs: Seq[String]) extends Query(strs: Seq[String]) {
	var goodDictionary: Seq[String] = Seq[String]()
	var badDictionary: Seq[String] = Seq[String]()

	var superWordAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()
	//var wordAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()

	def loadGood(){
		val bufferedSource = io.Source.fromFile("Dictionary.csv")
		for (line <- bufferedSource.getLines){
			val cols = line.split(",")
			val words = cols(0).split(" ")
			if (cols != Nil){
				for (w <- words){
					goodDictionary :+ w
				}
			}
		}
	}

	def loadBad(){
		val bufferedSource = io.Source.fromFile("Anti-dictionary.csv")
		for (line <- bufferedSource.getLines){
			val cols = line.split(",")
			for (c <- cols){
				badDictionary :+ c
			}
		}
	}

	override def assignWeights(): Unit = {
		
		for (s <- superWordAndWeight; g <- goodDictionary){
			if (s._1 == g){
				wordAndWeight :+ (s._1, 10.0)
			}
		}
		
		for (s <- superWordAndWeight; b <- badDictionary){
			if (s._1 == b){
				wordAndWeight :+ (s._1, 0.0)
			}
		}

		for (s <- strs; if (!goodDictionary.contains(s) && !badDictionary.contains(s))){
			wordAndWeight :+ (s, 1.0)
		}
	}
}
