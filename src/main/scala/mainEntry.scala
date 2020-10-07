package rjh.datageneration.com

import java.io._
import scala.util.Random

import java.util.Calendar

object mainEntry{

  val DUMP_FOLDER : String = "/users/robertherrick/Development/sparkDump/"

  def main(args : Array[String]) = {
    val ager = new AgePerformanceLoanFile()
    //generatePerformanceLoans(10122,new SimpleDate(2019,11,30),Some("PerformanceLoans20191130.csv"))
    ager.AgeOneMonth(DUMP_FOLDER + "PerformanceLoans20191130.csv",DUMP_FOLDER + "PerformanceLoans20191231.csv")
    ager.AgeOneMonth(DUMP_FOLDER + "PerformanceLoans20191231.csv",DUMP_FOLDER + "PerformanceLoans20200131.csv")
    ager.AgeOneMonth(DUMP_FOLDER + "PerformanceLoans20200131.csv",DUMP_FOLDER + "PerformanceLoans20200229.csv")
    ager.AgeOneMonth(DUMP_FOLDER + "PerformanceLoans20200229.csv",DUMP_FOLDER + "PerformanceLoans20190331.csv")
  }

  private def ageASetOfLoans(): Unit ={

  }

  private def generatePerformanceLoans(numberOfLoans : Int, asOfDate: SimpleDate, fileName : Option[String])={
    try {
      val loanFile = new File(DUMP_FOLDER + fileName.getOrElse("performanceLoans.csv"))
      val bw = new BufferedWriter(new FileWriter(loanFile))
      bw.write(PerformanceLoanGenerator.fileHeader)
      bw.newLine()
      (1 to numberOfLoans).foreach( id => {
        val newLoan = PerformanceLoanGenerator.get(id, asOfDate)
        //println(newLease.mkString("|"))
        bw.write(newLoan.mkString(","))
        bw.newLine()
      })
      bw.close()
    }
    catch {
      case e: java.io.IOException => println(s"IOException $e")
    }

  }

  private def generateUSLoans() ={
    val rand = new Random()
    println(s"starting at ${Calendar.getInstance.getTime}")
    val noLoans = 10000
    println(s"generating $noLoans loans")
    generateLoanData(USAutoLoanGenerator, noLoans, new SimpleDate(2018,7,22),Option("USAutoLoan10k.csv"))
    println(s"ending at ${Calendar.getInstance.getTime}")
  }

  private def generateLoans() = {
    val rand = new Random()
    println(s"starting at ${Calendar.getInstance.getTime}")
    val noLoans = 50000
    println(s"generating $noLoans loans")
    generateLoanData(AutoLoanGenerator, noLoans, new SimpleDate(2018,7,22),Option("5MillionLoans.csv"))
    println(s"ending at ${Calendar.getInstance.getTime}")
  }

  private def generateLeases(): Unit ={
    val rand = new Random()
    println(s"starting at ${Calendar.getInstance.getTime}")
    val noLeases = 10000 + rand.nextInt(1000)
    println(s"generating $noLeases leases")
    generateEquipmentLeaseData(noLeases, new SimpleDate(2018,7,22), Option("10KLeases.csv"))
    println(s"ending at ${Calendar.getInstance.getTime}")
  }

  private def generateLoanData(generator: LoanDataGenerator,  numberOfLoans : Int, asOfDate: SimpleDate, fileName : Option[String])={
    try {
      val loanFile = new File( DUMP_FOLDER + fileName.getOrElse("loans.csv"))
      val bw = new BufferedWriter(new FileWriter(loanFile))
      bw.write(generator.fileHeader.replaceAll("\\,","|"))
      bw.newLine()
      (1 to numberOfLoans).foreach( id => {
        val newLoan = generator.get(id, asOfDate)
        //println(newLease.mkString("|"))
        bw.write(newLoan.toString('|'))
        bw.newLine()
      })
      bw.close()
    }
    catch {
      case e: java.io.IOException => println(s"IOException $e")
    }

  }

  private def generateEquipmentLeaseData(numberOfLeases : Int, asOfDate: SimpleDate, fileName : Option[String])={
    try {
      val leaseFile = new File(DUMP_FOLDER + fileName.getOrElse("leases.csv"))
      val paymentFile = new File(DUMP_FOLDER + fileName.getOrElse("leases.txt") + ".pmtSched.csv")
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
