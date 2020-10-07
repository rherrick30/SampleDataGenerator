package rjh.datageneration.com

import java.io.File

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, SQLContext, SparkSession}
import rjh.datageneration.com.GeneratedLoan
import rjh.datageneration.com.USAutoLoanGenerator.rand

class AgePerformanceLoanFile {
  val spark = getSparkSession()

  def AgeOneMonth(oldFilename: String, newFileName: String, defaultRate: Double = 0.02, ppmtRate: Double = 0.05): Unit ={
    //import spark.implicits._

    val starterFile = spark.read.format("csv")
      .option("header","true")
      .option("inferSchema","true")
      .load(oldFilename)

    println(starterFile.columns.mkString(","))
    starterFile.createOrReplaceTempView("loanFile")

    spark.sql("select loanFile.*, rand() as seed from loanFile where currentBalance>0").createOrReplaceTempView("withRandomSeed")

    val ageSql : List[String] = List(
      s"""select
        |r.id,loanNumber,r.poolName,r.amountFinanced,r.currentBalance,r.defaultAmount,r.prepayment,r.loss,
        |CASE WHEN r.seed<${defaultRate} THEN round((0.5 + ${defaultRate}) * r.currentBalance, 2) ELSE 0 END AS recovery,
        |case when r.seed>=${defaultRate + ppmtRate} then round(r.apr * r.currentBalance / 12 ,2) else 0 end as scheduledInterest,
        |r.scheduledPrincipal + r.scheduledInterest as scheduledPrincipal,
        |r.monthsDelinquent,r.apr,r.seed
        from withRandomSeed r""".stripMargin
      ,
      s"""select
        |id,loanNumber,poolName,amountFinanced,
        |case when seed>=${defaultRate + ppmtRate} then ROUND(currentBalance - (scheduledPrincipal - scheduledInterest),2) else 0 end as currentBalance,
        |case when seed<${defaultRate} then currentBalance else 0 end as defaultAmount,
        |case when seed>=${defaultRate} and seed<${ppmtRate} then currentBalance else 0 end as prepayment,
        |case when seed<${defaultRate} then ROUND(currentBalance-recovery,2) else 0 end as loss,
        |recovery,scheduledInterest, case when seed>=${defaultRate+ppmtRate} then ROUND(scheduledPrincipal - scheduledInterest,2) else 0 end as scheduledPrincipal,
        |monthsDelinquent,apr
        |        from withRandomSeed""".stripMargin
    )

    for(s<-ageSql){
      spark.sql(s).createOrReplaceTempView("withRandomSeed")
    }

    val agedFrame = spark.sql("select * from withRandomSeed")

    writeDataFameToFile(agedFrame,newFileName)
  }

  def writeDataFameToFile( ouputDF : DataFrame, outputName : String)={
    val oldFile : File = new File(outputName)
    if (oldFile.exists()){
      if(oldFile.isDirectory){
        oldFile.listFiles.foreach(f=>{
          f.delete()
        })
      }
      oldFile.delete()
    }
    val outputFrame = ouputDF.repartition(1)
    outputFrame.write.format("csv").option("header","true").save(outputName)
  }

  private def getSparkSession() : SparkSession = {
    val spark =  SparkSession.builder()
        .master("local[*]")
        .config("spark.local.‌​dir","/users/rob_herrick/Development/SparkDump")
        .config("spark.eventLog.dir", "file:////c:////sparklogs")
        .config("spark.sql.warehouse.dir", "file:///c:/temp")
        .config("spark.scheduler.mode", "FAIR")
        .config("spark.sql.shuffle.partitions","6")
        .config("spark.driver.host", "localhost")
        .getOrCreate()

    spark.sparkContext.setLogLevel("ERROR")
    spark
  }

}
