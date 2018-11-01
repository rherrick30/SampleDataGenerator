package rjh.datageneration.com

import scala.util.Random

trait DataGeneratorBase {

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
  val optInsuranceCarrier = new SimpleChoiceField(insuranceOptions)
  val optCreditRating = new SimpleChoiceField(creditOptions)
  val optDeliquentMonths = new SimpleChoiceField(paymentStatusOptions)
  val optEuroAutoMakeModel = new SimpleChoiceField(euroAutoMakeModel)

  def r2( v : Double) : Double = Math.round( 100.0d * v ) / 100.0d

  def amortize(startDate: SimpleDate, periods: Int, amount: Double) : Seq[(SimpleDate, Double)] = {
    val allExceptLast = for {i <- (1 to periods - 1)} yield (startDate.addMonths(i), r2(amount/periods))
    allExceptLast :+ ((startDate.addMonths(periods), r2(amount - allExceptLast.map(p => p._2).sum)))
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
    (List("Alianz"), 934),
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

  // European Automobile models
  def euroAutoMakeModel : List[(List[String], Int)] = List(
    (List("Volkswagen","Golf"),483105)
    ,(List("Renault","Clio"),327395)
    ,(List("Volkswagen","Polo"),272061)
    ,(List("Ford","Fiesta"),254539)
    ,(List("Nissan","Qashqai"),247939)
    ,(List("Peugeot","208"),244615)
    ,(List("VW","Tiguan"),234916)
    ,(List("Opel / Vauxhall","Corsa"),232738)
    ,(List("Skoda","Octavia"),230116)
    ,(List("Opel / Vauxhall","Astra"),217813)
    ,(List("Ford","Focus"),214661)
    ,(List("Renault","Captur"),212768)
    ,(List("Citroen","C3"),207299)
    ,(List("Toyota","Yaris"),199182)
    ,(List("Dacia","Sandero"),196067)
    ,(List("Fiat","500"),189928)
    ,(List("Fiat","Panda"),187322)
    ,(List("VW","Passat"),184123)
    ,(List("Peugeot","2008"),180868)
    ,(List("Skoda","Fabia"),180136)
    ,(List("Mercedes Benz","C-Class"),176705)
    ,(List("Opel / Vauxhall","Mokka"),170384)
    ,(List("Renault","Megane"),168132)
    ,(List("Peugeot","3008"),166784)
    ,(List("Audi","A3"),164045)
  )

}
