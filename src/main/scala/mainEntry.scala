package rjh.datageneration.com

import java.io._
import scala.util.Random

import java.util.Calendar

object mainEntry{

  def main(args : Array[String]) = {
    val rand = new Random()
    println(s"starting at ${Calendar.getInstance.getTime}")
    val noLeases = 10000 + rand.nextInt(1000)
    println(s"generating $noLeases leases")
    generateEquipmentLeaseData(noLeases, new SimpleDate(2018,7,22))
    println(s"ending at ${Calendar.getInstance.getTime}")
  }



  private def generateEquipmentLeaseData(numberOfLeases : Int, asOfDate: SimpleDate)={
    try {
      val leaseFile = new File("/users/rob_herrick/development/SparkDump/leases.txt")
      val paymentFile = new File("/users/rob_herrick/development/SparkDump/payments.txt")
      val bw = new BufferedWriter(new FileWriter(leaseFile))
      val bwPayments = new BufferedWriter(new FileWriter(paymentFile))
      bw.write(EquipmentLeaseGenerator.fileHeader.replaceAll("\\,","|"))
      bw.newLine()
      (1 to numberOfLeases).foreach( id => {
        val (newLease, paymentSchedule) = EquipmentLeaseGenerator.get(id, asOfDate)
        //println(newLease.mkString("|"))
        bw.write(newLease.mkString("|"))
        bw.newLine()
        paymentSchedule.foreach(p => {
          bwPayments.write(s"$id|${p._1}|${p._2}")
          bwPayments.newLine()
        })
      })
      bw.close()
      bwPayments.close()
    }
    catch {
      case e: java.io.IOException => println(s"IOException $e")
    }

  }

  private def randomMultiVarTest() = {

    val test = new SimpleChoiceField(List(
      (List("London","UK","GBP"), 30),
      (List("Manchester","UK","GBP"), 30),
      (List("Paris","France","EUR"), 30),
      (List("Nice","France","EUR"), 30),
      (List("Frankfurt","Germany","EUR"), 30),
      (List("Hannover","Germany","EUR"), 30),
      (List("Berlin","Germany","EUR"), 30),
      (List("Mannheim","Germany","EUR"), 30)
    ))

    val List(city, country, currency) = test.next()
    println(s"$city in $country pays with $currency")
  }

  private def randomTest(): Unit ={
    val test = new SimpleChoiceField(List(
      (List("apple"), 30),
      (List("pear"), 10),
      (List("banana"), 20),
      (List("grapefruit"), 20),
      (List("lemon"), 20)
    ))

    val fruit = for {
      i <- (1 to 1000000)
    } yield test.next.head

    def count(f: String): Int = fruit.count(p=> p==f)

    println(s"there are ${count("apple")} apples")
    println(s"there are ${count("pear")} pear")
    println(s"there are ${count("banana")} banana")
    println(s"there are ${count("grapefruit")} grapefruit")
    println(s"there are ${count("lemon")} lemon")



  }

}
