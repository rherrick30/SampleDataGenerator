package rjh.datageneration.com

class SimpleChoiceField(options : List[(List[String],Int)]) extends Field {
  val total = options.map(o => o._2).reduce(_ + _)
  def next() : List[String] = {
    val choices = options zip options.map(o => o._2).scan(0)((a,b) => a+b)
    val choix = rand.nextInt(total)
    choices.filter( c => c._2 <= choix).reverse.head._1._1 //+ " (" + choix.toString +")"
  }
}
