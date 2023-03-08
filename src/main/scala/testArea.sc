import rjh.datageneration.com.SimpleDate


val origDate = new SimpleDate(2020,1,15)
val term = 60


val maturity = origDate.addMonths(term + 1)
