trait Weighted[A] {

  def getItems: Seq[A]
  def getWeights: Seq[Double]
  
  def sumIf(f: A => Boolean): Double = {
    // TODO: complete this method
    list.filter(f(getWeights(_))).foldLeft(0.0)( (x: Double, y: Double) => { x + y } )
  } 
}
