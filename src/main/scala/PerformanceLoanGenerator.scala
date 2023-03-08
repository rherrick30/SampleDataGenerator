package rjh.datageneration.com

import rjh.datageneration.com.USAutoLoanGenerator.{currencyConversions, earliestLoanDate, optLocale, rand}

object PerformanceLoanGenerator {

  val earliestLoanDate : SimpleDate = new SimpleDate(2022,1,1)
  val baseRate : Double = 0.04

  def fileHeader = "id,loanNumber,poolName,amountFinanced,currentBalance,defaultAmount,prepayment,loss,recovery,scheduledInterest,scheduledPrincipal,monthsDelinquent,apr,paymentDay"

  def get(_id : Int, asOf: SimpleDate) : List[String] = {

    val starterLoan = USAutoLoanGenerator.get(_id,asOf)

    val poolSeed = rand.nextFloat()
    val poolName = if(poolSeed<0.2){
      "2018-A"
    }else if(poolSeed<0.444){
      "2018-B"
    }else if(poolSeed<0.508){
      "2018-C"
    }else if(poolSeed<0.822){
      "Vanderbilt A"
    }else {
      "Vanderbilt B"
    }



    val perfIndicator = rand.nextFloat()
    val (defaultAmount,prepayment,loss,recovery,currentBalance) = if (perfIndicator<0.02){
      //defaulted
      val recoveryRate = 0.5 + rand.nextFloat() / 5
      val recovery = Math.round(starterLoan.currentPrincipalBalance * recoveryRate).asInstanceOf[Double]
      (starterLoan.currentPrincipalBalance,0.0d,starterLoan.currentPrincipalBalance - recovery,recovery,0.0d)
    }else if(perfIndicator<0.08){
      //prepaid in full
      (0.0d,starterLoan.currentPrincipalBalance,0.0d,0.0d,0.0d)
    } else{
      (0.0d,0.0d,0.0d,0.0d,starterLoan.currentPrincipalBalance)
    }
    val(scheduledInterest,scheduledPrincipal) = if(perfIndicator>0.08) {
      val int = Math.round(starterLoan.currentPrincipalBalance * starterLoan.interestRate * 100 / 12) / 100.0d
      val prin = starterLoan.nextPaymentAmountDue - int
      (int,prin)
    }else{
      (0.0d,0.0d)
    }

    val apr = Math.round((0.02d + (rand.nextFloat() / 10.0d)) * 100000.0d).toDouble / 100000.0d
    val paymentDay = rand.nextInt(28)

    def f(d: Double) :String = f"${d}%19.2f".trim
    List[String](
      starterLoan.id.toString(),starterLoan.loanNumber,poolName,f(starterLoan.amountFinanced),f(currentBalance)
      ,f(defaultAmount),f(prepayment),f(loss),f(recovery),f(scheduledInterest),f(scheduledPrincipal)
      ,starterLoan.paymentsBehind.toString,apr.toString,paymentDay.toString
    )
  }
}
