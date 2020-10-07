package rjh.datageneration.com

import java.util.Calendar

/* Because I think that the Java calendar class is a bit obscure, this is a nice wrapper
@year: the year
@month: month 1 - 12 (Rather than 0-11)
@day:  1 - 31 (or 30 or 28/29)
 */
class SimpleDate(year: Int, month: Int, day: Int) {
  val _internalCal : Calendar = Calendar.getInstance()
    _internalCal.set(year, month - 1, day)

  private def _fromCalendar( c: Calendar) : SimpleDate = new SimpleDate(c.get(Calendar.YEAR), c.get(Calendar.MONTH) + 1, c.get(Calendar.DAY_OF_MONTH))

  def addDays( daysToAdd : Int) : SimpleDate = new SimpleDate(Year,Month,Day + daysToAdd)
  def addMonths( monthsToAdd:Int) : SimpleDate = {
    val c = Calendar.getInstance()
    c.set(_internalCal.get(Calendar.YEAR), _internalCal.get(Calendar.MONTH), _internalCal.get(Calendar.DAY_OF_MONTH))
    c.add(Calendar.MONTH, monthsToAdd)
    _fromCalendar(c)
  }

  def monthsDifference (that: SimpleDate) = (this.Year - that.Year) * 12 + (this.Month - that.Month)

  def Year : Int = _internalCal.get(Calendar.YEAR)
  def Month: Int = _internalCal.get(Calendar.MONTH) + 1
  def Day: Int = _internalCal.get(Calendar.DAY_OF_MONTH)

  override def toString(): String =  f"${_internalCal.get(Calendar.YEAR)}%04d-${_internalCal.get(Calendar.MONTH) + 1}%02d-${_internalCal.get(Calendar.DAY_OF_MONTH)}%02d"

}
