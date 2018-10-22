package rjh.datageneration.com

import scala.util.Random

object EquipmentLeaseGenerator {

  val earliestLeaseDate : SimpleDate = new SimpleDate(2017,1,1)
  val rand = new Random()
  val optLocale = new SimpleChoiceField(cityOptions)
  val optIndustry = new SimpleChoiceField(industryOptions)
  val currencyConversions : Map[String, Double] = Map(
     "BGN" -> 1.95583d  // Bulgarian Lev
    ,"BYN" -> 2.29638d  // Belorusian Ruble
    ,"CHF" -> 1.17061d  // Swiss Franc
    ,"CZK" -> 25.8938d  // Czech Koruna
    ,"DKK" -> 7.45430d  // Danish Krone
    ,"EUR" -> 1.0d      // Euro (base Currency = $1.16)
    ,"GBP" -> 0.883041d // British Pound
    ,"HUF" -> 324.305d  // Hungarian Forint
    ,"NOK" -> 9.48413d  // Norweigan Krone
    ,"PLN" -> 4.31664d  // Polish Zloty
    ,"RON" -> 4.65506d  // Romanian Leu
    ,"RSD" -> 118.104d  // Serbian Dinar
    ,"RUB" -> 73.2547d  // Russian Ruble
    ,"SEK" -> 10.3814d  // Swedish Krona
    ,"TRY" -> 5.65668d  // Turkish Lira
    ,"UAH" -> 30.7755d  // Ukranian Hryvnia
  ).withDefaultValue(0.0d)
  val optResidualDelay = new SimpleChoiceField(List((List("0"), 50),(List("1"), 30),(List("2"), 20),(List("3"), 10)))
  val optInsuranceCarrier = new SimpleChoiceField(insuranceOptions)
  val optCreditRating = new SimpleChoiceField(creditOptions)
  val optPaymentSchedules = new SimpleChoiceField(List((List("1"),50),(List("2"),30),(List("3"),20)))
  val optDeliquentMonths = new SimpleChoiceField(paymentStatusOptions)

  def fileHeader = "id,leaseNumber,locale,country,currencyCode,industry,fundingBranch,equipmentValue,leaseDate," +
    "leaseTerm,residualDelay,residualAmount,residualDate," +
    "amountFinanced,insuranceCarrier,creditRating,serviceFeeRate,penaltyRate,paymentSchedules,purchaseOption," +
    "nextPaymentDueDate,nextPaymentAmountDue,paymentsBehind,paymentAhead,seasoning," +
    "remainingTerm,remainingRental"

  def r2( v : Double) : Double = Math.round( 100.0d * v ) / 100.0d
  def remainingSchedule(paidThru: SimpleDate, schedule : Seq[(SimpleDate, Double)]) : Seq[(SimpleDate,Double)] = for {
    p <- schedule
    if paidThru.monthsDifference(p._1) <= 0
  } yield p

  def get(_id : Int, asOf: SimpleDate) : (List[String],Seq[(SimpleDate,Double)]) = {

    val leaseNumber = java.util.UUID.randomUUID().toString.replace("-","")
    val List(metroArea, country, currencyCode) = optLocale.next()
    val List(industry,fundingBranch) = optIndustry.next()
    val leaseDate = new SimpleDate(earliestLeaseDate.Year, earliestLeaseDate.Month, rand.nextInt(200))

    val equipmentValue : Double =  Math.round( 100000 *  (10 + rand.nextInt(90)) * currencyConversions(currencyCode))
    val leaseTerm : Int = 36 + rand.nextInt(7) * 12

    val (residualDelay, residualAmount, residualDate) = if( rand.nextDouble()>0.4){
      val residualDelay = optResidualDelay.next.head.toInt
      (Some(residualDelay),Some( r2(equipmentValue * 0.1 * rand.nextDouble())),Some(leaseDate.addMonths(leaseTerm + residualDelay)))
    } else{
      (None, None, None)
    }
    val amountFinanced : Double = equipmentValue - residualAmount.getOrElse(0.0d)
    val insuranceCarrier = optInsuranceCarrier.next().head
    val List(creditRating,serviceFeeRate) = optCreditRating.next()
    val penaltyRate = "0.02"
    val pmtSchedules  = optPaymentSchedules.next.head.toInt

    def amortize(startDate: SimpleDate, periods: Int, amount: Double) : Seq[(SimpleDate, Double)] = {
      val allExceptLast = for {i <- (1 to periods - 1)} yield (startDate.addMonths(i), r2(amount/periods))
      allExceptLast :+ ((startDate.addMonths(periods), r2(amount - allExceptLast.map(p => p._2).sum)))
    }

    val pmtSchedule : Seq[(SimpleDate, Double)] = pmtSchedules match {
      case 1 => amortize(leaseDate,leaseTerm,amountFinanced)
      case schedules : Int => {
        var remainingTerm : Int = leaseTerm
        var remainingRent : Double = amountFinanced
        val pmtSched = (1 to schedules).map(i => {
          val rentBloc = r2(remainingRent * (if(schedules == i ) {1.0} else {rand.nextDouble() * 0.5 + 0.15}))
          val rentTerm = Math.round(remainingTerm * (if(schedules == i ) {1.0} else {rand.nextDouble() * 0.5 + 0.15})).toInt
          remainingRent  = remainingRent - rentBloc
          remainingTerm  = remainingTerm - rentTerm
          amortize(leaseDate.addMonths(remainingTerm), rentTerm, rentBloc)
        })
        pmtSched.flatten.reverse
      }
    }

    val purchaseOption = if(rand.nextDouble()<0.8) {"1"} else "0"
    val monthsPastDue = optDeliquentMonths.next().head.toInt
    val remainingPayments = remainingSchedule(asOf.addMonths(monthsPastDue), pmtSchedule)
    val (nextPaymentDueDate, nextPaymentAmountDue) = remainingPayments.head
    val (paymentsBehind,paymentAhead) = if(asOf.monthsDifference(nextPaymentDueDate)>0) {(asOf.monthsDifference(nextPaymentDueDate),0)} else (0,0-asOf.monthsDifference(nextPaymentDueDate))
    val seasoning  = asOf.monthsDifference(leaseDate) - 1
    val remainingRental = r2(remainingPayments.map(p=>p._2).sum)


    def f(d: Double) :String = f"${d}%19.2f".trim

    (
    List(_id.toString,leaseNumber,metroArea, country, currencyCode, industry,fundingBranch,f(equipmentValue),leaseDate.toString,
      leaseTerm.toString,residualDelay.getOrElse("").toString, residualAmount.getOrElse("").toString, residualDate.getOrElse("").toString,
      f(amountFinanced),insuranceCarrier,creditRating,serviceFeeRate,penaltyRate,pmtSchedules.toString,purchaseOption,
      nextPaymentDueDate.toString(),nextPaymentAmountDue.toString,paymentsBehind.toString,paymentAhead.toString,seasoning.toString,
      remainingPayments.length.toString,remainingRental.toString)
    ,pmtSchedule
    )
  }

  // Paid Ahead / Behind probablities
  def paymentStatusOptions : List[(List[String],Int)] = List(
    (List("-2"), 3),
    (List("-1"), 7),
    (List("0"), 65),
    (List("1"), 20),
    (List("2"), 5)
  )

  // Credit Options
  def creditOptions: List[(List[String],Int)] = List(
    (List("AAA","0.0025"), 5),
    (List("AA","0.0025"), 15),
    (List("AA","0.0027"), 20),
    (List("BBB","0.0030"), 30),
    (List("BB","0.0035"), 20),
    (List("B","0.0040"), 15),
    (List("CCC","0.0050"), 5)
  )

  //Insurance Options
  def insuranceOptions: List[(List[String], Int)] = List(
    (List("Axa"), 944),
    (List("Aliianz"), 934),
    (List("Prudential PLC"), 578),
    (List("Legal & General"), 574),
    (List("Generali"), 551),
    (List("Aviva"), 541),
    (List("Aegon"), 450),
    (List("CNP Assurances"), 443),
    (List("Zurich Insurance"), 382),
    (List("Munich Re"), 283)
  )

  // Metro Area, Country, Currency Code
  def cityOptions : List[(List[String],Int)] = List(
    (List("Vienna","Austria","EUR"),2025000)
    ,(List("Antwerp","Belgium","EUR"),1017197)
    ,(List("Brussels","Belgium","EUR"),1910000)
    ,(List("Sofia","Bulgaria","BGN"),1260000)
    ,(List("Prague","Czech Republic","CZK"),1390000)
    ,(List("Copenhagen","Denmark","DKK"),1420000)
    ,(List("Helsinki","Finland","EUR"),1120000)
    ,(List("Lille","France","EUR"),1240000)
    ,(List("Marseille","France","EUR"),1470000)
    ,(List("Lyon","France","EUR"),1470000)
    ,(List("Nice","France","EUR"),941000)
    ,(List("Paris","France","EUR"),10500000)
    ,(List("Berlin","Germany","EUR"),4325000)
    ,(List("Frankfurt","Germany","EUR"),1940000)
    ,(List("Hamburg","Germany","EUR"),2625000)
    ,(List("Munich","Germany","EUR"),2025000)
    ,(List("Nuremberg","Germany","EUR"),1060000)
    ,(List("Düsseldorf/Neuss","Germany","EUR"),1220000)
    ,(List("Cologne/Bonn","Germany","EUR"),1900000)
    ,(List("Ruhr","Germany","EUR"),4650000)
    ,(List("Stuttgart","Germany","EUR"),1980000)
    ,(List("Athens","Greece","EUR"),3775000)
    ,(List("Thessaloniki","Greece","EUR"),837000)
    ,(List("Budapest","Hungary","HUF"),2525000)
    ,(List("Dublin","Ireland","EUR"),1274000)
    ,(List("Milan","Italy","EUR"),8875000)
    ,(List("Naples","Italy","EUR"),6890000)
    ,(List("Rome","Italy","EUR"),5580000)
    ,(List("Turin","Italy","EUR"),1690000)
    ,(List("Amsterdam","Netherlands","EUR"),1970000)
    ,(List("The Hague","Netherlands","EUR"),626000)
    ,(List("Rotterdam","Netherlands","EUR"),2900000)
    ,(List("Oslo","Norway","NOK"),1130000)
    ,(List("Katowice","Poland","PLN"),2450000)
    ,(List("Kraków","Poland","PLN"),756000)
    ,(List("Warsaw","Poland","PLN"),2225000)
    ,(List("Lisbon","Portugal","EUR"),2575000)
    ,(List("Porto","Portugal","EUR"),1240000)
    ,(List("Belgrade","Serbia","RSD"),1750000)
    ,(List("Barcelona","Spain","EUR"),4500000)
    ,(List("Madrid","Spain","EUR"),5400000)
    ,(List("Seville","Spain","EUR"),1340000)
    ,(List("Valencia","Spain","EUR"),1780000)
    ,(List("Stockholm","Sweden","SEK"),2075000)
    ,(List("Zürich","Switzerland","CHF"),1180000)
    ,(List("Birmingham","United Kingdom","GBP"),2650000)
    ,(List("Glasgow","United Kingdom","GBP"),1430000)
    ,(List("Bradford/Leeds","United Kingdom","GBP"),2250000)
    ,(List("Liverpool","United Kingdom","GBP"),1350000)
    ,(List("London","United Kingdom","GBP"),12500000)
    ,(List("Manchester","United Kingdom","GBP"),2625000)
    ,(List("Newcastle","United Kingdom","GBP"),891000)
    ,(List("Sheffield","United Kingdom","GBP"),1330000)
    ,(List("Bucharest","Romania","RON"),2075000)
  )

  // Industry, Funding Branch
  def industryOptions : List[(List[String],Int)] = List(
    (List("Medical-Diagnostic Oncology","Oxford Group"), 5000)
    ,(List("Medical-Hemotology","Oxford Group"), 100)
    ,(List("Medical-Cryotherapy","Oxford Group"), 105)
    ,(List("Medical-Laser Surgery","Oxford Group"), 3000)
    ,(List("Medical-X Ray","Oxford Group"), 13000)
    ,(List("Medical-Diagnostic Cardiology","Oxford Group"), 5100)
    ,(List("Medical-MRI","Oxford Group"), 14000)
    ,(List("Farming","Agro Europe"), 51000)
    ,(List("Manufacturing-Aerospace","Avidias"), 1000)
    ,(List("Manufacturing-Textile","Avidias"), 100)
    ,(List("Mining","Avidias"), 2000)
    ,(List("Manufacturing-Other","Avidias"), 3000)
    ,(List("Manufacturing-Automotive","Avidias"), 10000)
  )

}
