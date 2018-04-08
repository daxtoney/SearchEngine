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