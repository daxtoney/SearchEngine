class Query (strs: Seq[String]) extends Weighted[String] {
	var wordAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()

	def getItems(): Seq[String] = {
		strs
	}

	def getWeights(): Seq[Double] = {
		assignWeights()
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
	var wAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()

	override def getWeights(): Seq[Double] = {
		if (wAndWeight.length < strs.length){
			loadGood()
			loadBad()
			assignWeights()
		}
		
		val ret = for(weight <- wAndWeight) yield weight._2
		ret
	}

	def loadGood(): Seq[String] = {
		if (goodDictionary.length > 0){
			goodDictionary
		}
		val bufferedSource = io.Source.fromFile("Dictionary.csv")
		for (line <- bufferedSource.getLines){
			val cols = line.split(",")
			val words = cols(0).split(" ")
			if (cols != Nil){
				for (w <- words){
					//println(w)
					goodDictionary = goodDictionary :+ w
				}
			}
			//println(line)
		}
		//println(goodDictionary)
		
		goodDictionary
	}

	def loadBad(): Seq[String] = {
		if (badDictionary.length > 0){
			badDictionary
		}
		val bufferedSource = io.Source.fromFile("Anti-dictionary.csv")
		for (line <- bufferedSource.getLines){
			val cols = line.split(",")
			for (c <- cols){
				//println(c)
				badDictionary = badDictionary :+ c
			}
			//println(line)
		}
		
		badDictionary
	}

	override def assignWeights(): Unit = {
		if (strs.length > wAndWeight.length){
			superWordAndWeight = strs.map((_, 4.0))
		
			/*for (s <- superWordAndWeight; g <- goodDictionary){
				if (s._1 == g){
					wAndWeight = wAndWeight :+ (s._1, 10.0)
				}
			}
		
			for (s <- superWordAndWeight; b <- badDictionary){
				if (s._1 == b){
					wAndWeight = wAndWeight :+ (s._1, 1.0)
				}
			}

			for (s <- strs; if (!goodDictionary.contains(s) && !badDictionary.contains(s))){
				wAndWeight = wAndWeight :+ (s, 4.0)
			}*/

			for (s <- superWordAndWeight){
                if (goodDictionary.contains(s._1)){
                    wAndWeight = wAndWeight :+ (s._1, 10.0)
                }
                else if (badDictionary.contains(s._1)){
                    wAndWeight = wAndWeight :+ (s._1, 1.0)
                }
                else{
                    wAndWeight = wAndWeight :+ (s._1, 3.0)
                }
            }

		/*for (w <- wAndWeight){
			println(w)
		}*/
			print("This is my Size: "+wAndWeight.length)
		}
	}
}
