package rjh.datageneration.com

object AutoLoanGenerator extends DataGeneratorBase {

  val earliestLoanDate : SimpleDate = new SimpleDate(2017,1,1)
  val optBalloonTerm = new SimpleChoiceField(List((List("0"), 50),(List("1"), 30),(List("2"), 20),(List("3"), 10)))
  val baseRate : Double = 0.04


  def fileHeader = "id,loanNumber,locale,country,currencyCode,autoMSRP,loanDate," +
    "loanTerm,balloonTerm,baloonAmount,balloonDate," +
    "amountFinanced,insuranceCarrier,creditRating,serviceFeeRate,penaltyRate," +
    "nextPaymentDueDate,nextPaymentAmountDue,paymentsBehind,paymentsAhead,seasoning," +
    "remainingTerm,balloonPrincipal,interestRate,currentPrincipalBalance,Make,Model"

  def remainingSchedule(paidThru: SimpleDate, schedule : Seq[(SimpleDate, Double)]) : Seq[(SimpleDate,Double)] = for {
    p <- schedule
    if paidThru.monthsDifference(p._1) <= 0
  } yield p

  def get(_id : Int, asOf: SimpleDate) : List[String] = {

    val loanNumber = java.util.UUID.randomUUID().toString.replace("-","")
    val List(metroArea, country, currencyCode) = optLocale.next()
    val loanDate = new SimpleDate(earliestLoanDate.Year, earliestLoanDate.Month, rand.nextInt(200))

    val autoMSRP : Double =  Math.round( 50 *  (40 + rand.nextInt(160)) * currencyConversions(currencyCode))
    val loanTerm : Int = 36 + rand.nextInt(7) * 12

    val (balloonTerm, balloonPrincipal, balloonDate) = if( rand.nextDouble()>0.4){
      val residualDelay = optBalloonTerm.next.head.toInt
      (Some(residualDelay),Some( r2(autoMSRP * 0.1 * rand.nextDouble())),Some(loanDate.addMonths(loanTerm + residualDelay)))
    } else{
      (None, None, None)
    }
    val amountFinanced : Double = autoMSRP - balloonPrincipal.getOrElse(0.0d)
    val insuranceCarrier = optInsuranceCarrier.next().head
    val List(creditRating,serviceFeeRate) = optCreditRating.next()
    val penaltyRate = "0.02"

    def amortize(startDate: SimpleDate, periods: Int, amount: Double) : Seq[(SimpleDate, Double)] = {
      val allExceptLast = for {i <- (1 to periods - 1)} yield (startDate.addMonths(i), r2(amount/periods))
      allExceptLast :+ ((startDate.addMonths(periods), r2(amount - allExceptLast.map(p => p._2).sum)))
    }

    val pmtSchedule : Seq[(SimpleDate, Double)] = amortize(loanDate,loanTerm,amountFinanced)
    val monthsPastDue = optDeliquentMonths.next().head.toInt
    val remainingPayments = remainingSchedule(asOf.addMonths(monthsPastDue), pmtSchedule)
    val (nextPaymentDueDate, nextPaymentAmountDue) = remainingPayments.head
    val (paymentsBehind,paymentAhead) = if(asOf.monthsDifference(nextPaymentDueDate)>0) {(asOf.monthsDifference(nextPaymentDueDate),0)} else (0,0-asOf.monthsDifference(nextPaymentDueDate))
    val seasoning  = asOf.monthsDifference(loanDate) - 1

    val interestRate : Double = baseRate + serviceFeeRate.toDouble
    val paymentAmount : Double = r2((interestRate.toDouble / 12 * amountFinanced.toDouble)/(1 - Math.pow(1 + interestRate/12,-1 * loanTerm)))
    //r2((interestRate.toDouble / 12.0 * -1 * amountFinanced.toDouble)/(1 - Math.pow(1+interestRate.toDouble / 12.0, (-1.0 * loanTerm)) ))

    val balloonAmount : Double = r2(balloonPrincipal.getOrElse(0d) * balloonTerm.getOrElse(0).toDouble) * interestRate.toDouble + balloonPrincipal.getOrElse(0d)
    val currentPrincipalBalance = r2(paymentAmount * (1 - Math.pow(1 + interestRate/12,-1 * remainingPayments.length)) / (interestRate/12))
    val List(make,model) = optEuroAutoMakeModel.next()

    def f(d: Double) :String = f"${d}%19.2f".trim

    List(_id.toString,loanNumber,metroArea, country, currencyCode,f(autoMSRP),loanDate.toString,
      loanTerm.toString,balloonTerm.getOrElse("").toString, f(balloonAmount), balloonDate.getOrElse("").toString,
      f(amountFinanced),insuranceCarrier,creditRating,serviceFeeRate,penaltyRate,
      nextPaymentDueDate.toString(),paymentAmount.toString,paymentsBehind.toString,paymentAhead.toString,seasoning.toString,
      remainingPayments.length.toString,balloonPrincipal.getOrElse("").toString,interestRate.toString,currentPrincipalBalance.toString,make,model)

  }

}
