package rjh.datageneration.com

case class PerformanceLoan(
                            id: String,
                            loanNumber: String,
                            poolName: String,
                            amountFinanced: String,
                            currentBalance: String,
                            defaultAmount: String,
                            prepayment: String,
                            loss: String,
                            recovery: String,
                            scheduledInterest: String,
                            scheduledPrincipal: String,
                            monthsDelinquent: String,
                            apr: String,
                            paymentDay: String
                          ){
  def toCsv : String = s"$id,$loanNumber,$poolName,$amountFinanced,$currentBalance,$defaultAmount,$prepayment,$loss,$recovery,$scheduledInterest,$scheduledPrincipal,$monthsDelinquent,$apr,$paymentDay"
}
