trait Weighted[A] {

  def getItems: Seq[A]
  def getWeights: Seq[Double]
  
  def sumIf(f: A => Boolean): Double = {
    // TODO: complete this method

    // Is this right 	
    val together = getItems.zip(getWeights)
    together.filter( (x: (A, Double)) => f(x._1 ) ).foldLeft(0.0)( (x: Double, y: (A, Double) ) => { x + y._2 } )
  } 
}
