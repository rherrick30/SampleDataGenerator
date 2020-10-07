package rjh.datageneration.com

object EquipmentLeaseGenerator extends DataGeneratorBase {

  val earliestLeaseDate : SimpleDate = new SimpleDate(2017,1,1)
  val optResidualDelay = new SimpleChoiceField(List((List("0"), 50),(List("1"), 30),(List("2"), 20),(List("3"), 10)))
  val optPaymentSchedules = new SimpleChoiceField(List((List("1"),50),(List("2"),30),(List("3"),20)))

  def fileHeader = "id,leaseNumber,poolName,locale,country,currencyCode,industry,fundingBranch,equipmentValue,leaseDate," +
    "leaseTerm,residualDelay,residualAmount,residualDate," +
    "amountFinanced,insuranceCarrier,creditRating,serviceFeeRate,penaltyRate,paymentSchedules,purchaseOption," +
    "nextPaymentDueDate,nextPaymentAmountDue,paymentsBehind,paymentAhead,seasoning," +
    "remainingTerm,remainingRental"



  def remainingSchedule(paidThru: SimpleDate, schedule : Seq[(SimpleDate, Double)]) : Seq[(SimpleDate,Double)] = for {
    p <- schedule
    if paidThru.monthsDifference(p._1) <= 0
  } yield p

  def get(_id : Int, asOf: SimpleDate) : (List[String],Seq[(SimpleDate,Double)]) = {

    val leaseNumber = java.util.UUID.randomUUID().toString.replace("-","")
    val poolName = if (rand.nextInt() < 10)  "pledged" else "unsold"
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
    List(_id.toString,leaseNumber,poolName,metroArea, country, currencyCode, industry,fundingBranch,f(equipmentValue),leaseDate.toString,
      leaseTerm.toString,residualDelay.getOrElse("").toString, residualAmount.getOrElse("").toString, residualDate.getOrElse("").toString,
      f(amountFinanced),insuranceCarrier,creditRating,serviceFeeRate,penaltyRate,pmtSchedules.toString,purchaseOption,
      nextPaymentDueDate.toString(),nextPaymentAmountDue.toString,paymentsBehind.toString,paymentAhead.toString,seasoning.toString,
      remainingPayments.length.toString,remainingRental.toString)
    ,pmtSchedule
    )
  }


}
