package rjh.datageneration.com

import scala.util.Random

abstract class Field() {
  val rand = new Random()
  def next : List[String]
}
