package rjh.datageneration.com

case class GeneratedLoan(
                          id: BigInt,
                          loanNumber: String,
                          poolName: String,
                          metroArea: String,
                          region: String,
                          currencyCode: String,
                          autoMSRP: Double,
                          loanDate: SimpleDate,
                          loanTerm: Int,
                          balloonTerm: Option[Int],
                          balloonAmount: Double,
                          balloonDate: Option[SimpleDate],
                          amountFinanced: Double,
                          insuranceCarrier: String,
                          creditRating: String,
                          serviceFeeRate: Double,
                          penaltyRate: Double,
                          nextPaymentDueDate: SimpleDate,
                          nextPaymentAmountDue: Double,
                          paymentsBehind: Int,
                          paymentsAhead: Int,
                          seasoning: Int,
                          remainingTerm: Int,
                          balloonPrincipal: Option[Double],
                          interestRate: Double,
                          currentPrincipalBalance: Double,
                          make: String,
                          model: String
) {

  def f(d: Double) :String = f"${d}%19.2f".trim

  override def toString(): String = {
    s"""${id.toString},$loanNumber,$poolName,$metroArea,$region,$currencyCode,${f(autoMSRP)},${loanDate.toString},
    ${loanTerm.toString},${balloonTerm.getOrElse("").toString},${f(balloonAmount)},${balloonDate.getOrElse("").toString},
    ${f(amountFinanced)},$insuranceCarrier,$creditRating,$serviceFeeRate,$penaltyRate,
    ${nextPaymentDueDate.toString()},${nextPaymentAmountDue.toString},${paymentsBehind.toString},${paymentsAhead.toString},${seasoning.toString},
    ${remainingTerm.toString},${balloonPrincipal.getOrElse("").toString},${interestRate.toString},${currentPrincipalBalance.toString},$make,$model"""
  }

  def age(g: GeneratedLoan) : GeneratedLoan ={
    val newSeas = Math.min(g.loanTerm, g.remainingTerm + 1)
    val newRemTerm = Math.max(0, g.remainingTerm - 1)
    val nextDD = g.nextPaymentDueDate.addMonths(1)
    val prinBal = Math.round(Math.min(0, 100 * ( g.currentPrincipalBalance - (g.nextPaymentAmountDue - g.interestRate / 12 * g.currentPrincipalBalance)))) / 100
    GeneratedLoan(
      g.id,g.loanNumber,g.poolName,g.metroArea,g.region,g.currencyCode,g.autoMSRP,g.loanDate,g.loanTerm,g.balloonTerm,g.balloonAmount,g.balloonDate, g.amountFinanced, g.insuranceCarrier,
      g.creditRating,g.serviceFeeRate, g.penaltyRate,nextDD,g.nextPaymentAmountDue, g.paymentsBehind,g.paymentsAhead,newSeas,newRemTerm,g.balloonPrincipal,g.interestRate,
      prinBal,g.make,g.model
    )
  }

  /*

  Not sure I'll need it....
  def fromString(s: String) : GeneratedLoan = {
    val fields
  }
  */
}


